module Llm.Claude
  ( mkCategorizer
  , parseClaudeResponse
  ) where

import Config (Config (..))
import Data.Aeson
  ( FromJSON (..)
  , Value (..)
  , decode
  , encode
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Llm.Categorizer
  ( CategorizeRequest (..)
  , Categorizer (..)
  , LlmError (..)
  , RuleHint (..)
  , Suggestion (..)
  , TxForLlm (..)
  )
import Network.HTTP.Client
  ( Manager
  , RequestBody (..)
  , httpLbs
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types (hContentType, statusCode)
import Numeric (showFFloat)
import Rules (CandidateCategory (..))

mkCategorizer :: Manager -> Config -> Categorizer
mkCategorizer manager cfg =
  Categorizer (categorizeBatches manager cfg)

categorizeBatches :: Manager -> Config -> CategorizeRequest -> IO (Either LlmError [Suggestion])
categorizeBatches _ _ req
  | null (crTransactions req) = pure (Right [])
categorizeBatches manager cfg req =
  go [] (chunksOf (cfgLlmBatchSize cfg) (crTransactions req))
  where
    go acc [] = pure (Right acc)
    go acc (batch : rest) = do
      result <- oneBatch manager cfg req {crTransactions = batch}
      case result of
        Left err -> pure (Left err)
        Right suggestions -> go (acc ++ suggestions) rest

oneBatch :: Manager -> Config -> CategorizeRequest -> IO (Either LlmError [Suggestion])
oneBatch manager cfg req = do
  initReq <- parseRequest "POST https://api.anthropic.com/v1/messages"
  let payload = encode (claudeRequest cfg req)
      request =
        initReq
          { requestHeaders =
              [ (hContentType, "application/json")
              , ("x-api-key", encodeUtf8 (cfgAnthropicApiKey cfg))
              , ("anthropic-version", "2023-06-01")
              ]
          , requestBody = RequestBodyLBS payload
          , responseTimeout = responseTimeoutMicro (120 * 1000000)
          }
  response <- httpLbs request manager
  let code = statusCode (responseStatus response)
      body = responseBody response
  if code >= 200 && code < 300
    then
      pure $
        case parseClaudeResponse body of
          Left err ->
            Left
              LlmError
                { llmStatus = Nothing
                , llmBody = T.pack err <> " | " <> T.pack (LBS8.unpack body)
                }
          Right suggestions -> Right suggestions
    else
      pure $
        Left
          LlmError
            { llmStatus = Just code
            , llmBody = T.pack (LBS8.unpack body)
            }

claudeRequest :: Config -> CategorizeRequest -> Value
claudeRequest cfg req =
  object
    [ "model" .= cfgAnthropicModel cfg
    , "max_tokens" .= (8192 :: Int)
    , "system" .= systemPrompt req
    , "messages"
        .= [ object
               [ "role" .= ("user" :: Text)
               , "content" .= userPrompt req
               ]
           ]
    , "tools" .= [recordTool (crCategories req)]
    , "tool_choice"
        .= object
          [ "type" .= ("tool" :: Text)
          , "name" .= ("record_categorizations" :: Text)
          ]
    ]

-- Haiku 5.5 rejects temperature/top_p/top_k; forced tool use is the structured output path.
recordTool :: [CandidateCategory] -> Value
recordTool cats =
  object
    [ "name" .= ("record_categorizations" :: Text)
    , "description"
        .= ("Record a category suggestion for every transaction." :: Text)
    , "input_schema"
        .= object
          [ "type" .= ("object" :: Text)
          , "properties"
              .= object
                [ "suggestions"
                    .= object
                      [ "type" .= ("array" :: Text)
                      , "items" .= suggestionItemSchema cats
                      ]
                ]
          , "required" .= (["suggestions"] :: [Text])
          ]
    ]

suggestionItemSchema :: [CandidateCategory] -> Value
suggestionItemSchema cats =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "transactionId" .= object ["type" .= ("string" :: Text)]
          , "categoryId"
              .= object
                [ "type" .= ("string" :: Text)
                , "enum" .= map ccId cats
                ]
          , "confidence" .= object ["type" .= ("number" :: Text)]
          , "reason" .= object ["type" .= ("string" :: Text)]
          ]
    , "required"
        .= (["transactionId", "categoryId", "confidence", "reason"] :: [Text])
    ]

systemPrompt :: CategorizeRequest -> Text
systemPrompt req =
  T.unlines $
    [ "You categorize YNAB bank transactions."
    , "Pick exactly one categoryId from the provided list for every transaction."
    , "Vendor rules are usually right but not guaranteed. Treat a matching rule as a strong prior, then judge from payee, amount, and memo."
    , "Return calibrated confidence in [0,1]:"
    , "- 0.9+ when the payee clearly matches the category"
    , "- around 0.5 when uncertain"
    , "- below 0.4 when guessing"
    , "Reason must be at most 80 characters."
    , "Call record_categorizations once with a suggestion for every transaction id you were given."
    , "Do not invent transaction ids or category ids."
    , ""
    , "Categories:"
    ]
      ++ map formatCategory (crCategories req)
      ++ if null (crHints req)
        then []
        else "" : "User hints:" : map ("- " <>) (crHints req)

formatCategory :: CandidateCategory -> Text
formatCategory c =
  "- id=" <> ccId c <> " group=" <> ccGroup c <> " name=" <> ccName c

userPrompt :: CategorizeRequest -> Text
userPrompt req =
  T.unlines $
    "Categorize these transactions:"
      : map formatTx (crTransactions req)

formatTx :: TxForLlm -> Text
formatTx tx =
  T.intercalate
    " | "
    [ "id=" <> tlId tx
    , "date=" <> tlDate tx
    , "payee=" <> tlPayee tx
    , "amount=" <> T.pack (showFFloat (Just 2) dollars "")
    , "account=" <> tlAccountName tx
    , "existing=" <> maybe "-" id (tlExistingCategory tx)
    , "memo=" <> maybe "-" id (tlMemo tx)
    , "rule=" <> ruleText (tlSuggestedRule tx)
    ]
  where
    dollars = fromIntegral (tlAmountMilliunits tx) / 1000.0 :: Double

ruleText :: Maybe RuleHint -> Text
ruleText Nothing = "-"
ruleText (Just hint) =
  "suggested category (usually right, not guaranteed): "
    <> rhCategoryName hint
    <> maybe "" (\n -> " (" <> n <> ")") (rhNote hint)

data ClaudeMessage = ClaudeMessage
  { cmContent :: [ContentBlock]
  }

instance FromJSON ClaudeMessage where
  parseJSON = withObject "ClaudeMessage" $ \o ->
    ClaudeMessage <$> o .: "content"

data ContentBlock
  = ToolUse Text Value
  | OtherBlock

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "tool_use" -> ToolUse <$> o .: "name" <*> o .: "input"
      _ -> pure OtherBlock

newtype SuggestionsPayload = SuggestionsPayload
  { spSuggestions :: [Suggestion]
  }

instance FromJSON SuggestionsPayload where
  parseJSON = withObject "SuggestionsPayload" $ \o ->
    SuggestionsPayload <$> o .: "suggestions"

parseClaudeResponse :: LBS.ByteString -> Either String [Suggestion]
parseClaudeResponse body =
  case decode body of
    Nothing -> Left "Claude response was not valid JSON"
    Just value -> parseEither parseMessage value

parseMessage :: Value -> Parser [Suggestion]
parseMessage value = do
  msg <- parseJSON value
  case [input | ToolUse name input <- cmContent msg, name == "record_categorizations"] of
    [] -> fail "Claude response had no record_categorizations tool_use block"
    (input : _) -> spSuggestions <$> parseJSON input

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0 = [xs]
  | null xs = []
  | otherwise =
      let (a, b) = splitAt n xs
       in a : chunksOf n b
