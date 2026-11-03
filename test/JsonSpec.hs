module JsonSpec (spec) where

import Data.Aeson (FromJSON (..), Value (..), decode, encode, object, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Llm.Categorizer (Suggestion (..))
import Llm.Claude (parseClaudeResponse)
import Test.Hspec
import Ynab.Types

spec :: Spec
spec = do
  describe "YNAB JSON" $ do
    it "parses a categories envelope" $ do
      let raw =
            LBS8.pack
              "{\"data\":{\"category_groups\":[{\"id\":\"g1\",\"name\":\"Food\",\"hidden\":false,\"deleted\":false,\"categories\":[{\"id\":\"c1\",\"category_group_id\":\"g1\",\"name\":\"Groceries\",\"hidden\":false,\"deleted\":false}]}],\"server_knowledge\":1}}"
      case decode raw of
        Just (CategoriesProbe [g])
          | [c] <- cgCategories g -> do
              cgName g `shouldBe` "Food"
              catName c `shouldBe` "Groceries"
        Just (CategoriesProbe groups) ->
          expectationFailure ("unexpected groups: " <> show (length groups))
        Nothing -> expectationFailure "failed to parse categories envelope"

    it "parses an uncategorized transaction" $ do
      case decode transactionJson of
        Just tx -> do
          txId tx `shouldBe` "tx1"
          txAmount tx `shouldBe` -84230
          txPayeeName tx `shouldBe` Just "COSTCO #123"
          txApproved tx `shouldBe` False
          txFlagColor tx `shouldBe` Nothing
        Nothing -> expectationFailure "failed to parse transaction"

    it "encodes a patch without an approved field" $ do
      let patch =
            TransactionPatch
              { tpId = "tx1"
              , tpCategoryId = Just "c1"
              , tpMemo = "[auto 0.93 confident] bulk food || "
              , tpFlagColor = FlagGreen
              }
      case decode (encode patch) of
        Just (Object o) -> do
          KM.member "approved" o `shouldBe` False
          KM.lookup "id" o `shouldBe` Just (String "tx1")
          KM.lookup "category_id" o `shouldBe` Just (String "c1")
          KM.lookup "flag_color" o `shouldBe` Just (String "green")
        _ -> expectationFailure "expected a JSON object"

    it "omits category_id when the patch has none" $ do
      let patch =
            TransactionPatch
              { tpId = "tx1"
              , tpCategoryId = Nothing
              , tpMemo = "[auto n/a unsupported] cannot set category || "
              , tpFlagColor = FlagYellow
              }
      case decode (encode patch) of
        Just (Object o) -> KM.member "category_id" o `shouldBe` False
        _ -> expectationFailure "expected a JSON object"

  describe "Anthropic JSON" $ do
    it "parses a tool_use content block into suggestions" $ do
      case parseClaudeResponse claudeJson of
        Left err -> expectationFailure err
        Right [s] -> do
          sTransactionId s `shouldBe` "tx1"
          sCategoryId s `shouldBe` "cat-groc"
          sConfidence s `shouldBe` 0.93
          sReason s `shouldBe` "bulk food"
        Right other -> expectationFailure ("unexpected suggestions: " <> show other)

newtype CategoriesProbe = CategoriesProbe [YnabCategoryGroup]

instance FromJSON CategoriesProbe where
  parseJSON = withObject "CategoriesProbe" $ \o -> do
    d <- o .: "data"
    CategoriesProbe <$> d .: "category_groups"

transactionJson :: LBS8.ByteString
transactionJson =
  encode $
    object
      [ "id" .= ("tx1" :: String)
      , "date" .= ("2026-11-01" :: String)
      , "amount" .= (-84230 :: Integer)
      , "approved" .= False
      , "account_id" .= ("acc" :: String)
      , "account_name" .= ("Chase" :: String)
      , "payee_name" .= ("COSTCO #123" :: String)
      , "deleted" .= False
      , "subtransactions" .= ([] :: [Value])
      ]

claudeJson :: LBS8.ByteString
claudeJson =
  encode $
    object
      [ "id" .= ("msg_1" :: String)
      , "type" .= ("message" :: String)
      , "role" .= ("assistant" :: String)
      , "content"
          .= [ object
                 [ "type" .= ("tool_use" :: String)
                 , "id" .= ("toolu_1" :: String)
                 , "name" .= ("record_categorizations" :: String)
                 , "input"
                     .= object
                       [ "suggestions"
                           .= [ object
                                  [ "transactionId" .= ("tx1" :: String)
                                  , "categoryId" .= ("cat-groc" :: String)
                                  , "confidence" .= (0.93 :: Double)
                                  , "reason" .= ("bulk food" :: String)
                                  ]
                              ]
                       ]
                 ]
             ]
      ]
