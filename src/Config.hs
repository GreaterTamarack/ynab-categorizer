module Config
  ( Config (..)
  , loadEnvFile
  , loadConfig
  ) where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)
import Ynab.Types (FlagColor (..), parseFlagColor)

data Config = Config
  { cfgYnabAccessToken :: Text
  , cfgYnabPlanId :: Text
  , cfgAnthropicApiKey :: Text
  , cfgAnthropicModel :: Text
  , cfgApiKey :: Text
  , cfgConfidenceThreshold :: Double
  , cfgConfidentFlagColor :: FlagColor
  , cfgReviewFlagColor :: FlagColor
  , cfgLlmBatchSize :: Int
  , cfgRulesFile :: FilePath
  , cfgPort :: Int
  }

loadEnvFile :: IO ()
loadEnvFile = onMissingFile (loadFile defaultConfig) (pure ())

loadConfig :: IO Config
loadConfig = do
  ynabToken <- requireEnv "YNAB_ACCESS_TOKEN"
  ynabPlan <- requireEnv "YNAB_PLAN_ID"
  anthropicKey <- requireEnv "ANTHROPIC_API_KEY"
  anthropicModel <- optionalEnv "ANTHROPIC_MODEL" "claude-haiku-5-5"
  apiKey <- requireEnv "API_KEY"
  threshold <- readEnv "CONFIDENCE_THRESHOLD" 0.8
  confidentFlag <- flagEnv "CONFIDENT_FLAG_COLOR" FlagGreen
  reviewFlag <- flagEnv "REVIEW_FLAG_COLOR" FlagYellow
  batchSize <- readEnv "LLM_BATCH_SIZE" (25 :: Int)
  rulesFile <- optionalEnv "RULES_FILE" "config/vendor-rules.yaml"
  port <- readEnv "PORT" (3000 :: Int)
  if batchSize < 1
    then die "LLM_BATCH_SIZE must be at least 1."
    else
      pure
        Config
          { cfgYnabAccessToken = T.pack ynabToken
          , cfgYnabPlanId = T.pack ynabPlan
          , cfgAnthropicApiKey = T.pack anthropicKey
          , cfgAnthropicModel = T.pack anthropicModel
          , cfgApiKey = T.pack apiKey
          , cfgConfidenceThreshold = threshold
          , cfgConfidentFlagColor = confidentFlag
          , cfgReviewFlagColor = reviewFlag
          , cfgLlmBatchSize = batchSize
          , cfgRulesFile = rulesFile
          , cfgPort = port
          }

flagEnv :: String -> FlagColor -> IO FlagColor
flagEnv key def = do
  found <- lookupEnv key
  case found >>= nonEmpty of
    Nothing -> pure def
    Just raw ->
      case parseFlagColor raw of
        Just color -> pure color
        Nothing ->
          die $
            key
              <> " must be one of red, orange, yellow, green, blue, purple."

requireEnv :: String -> IO String
requireEnv key = do
  found <- lookupEnv key
  case found >>= nonEmpty of
    Just value -> pure value
    Nothing -> die $ "Set " <> key <> " in .env or the environment."

optionalEnv :: String -> String -> IO String
optionalEnv key def = do
  found <- lookupEnv key
  case found >>= nonEmpty of
    Just value -> pure value
    Nothing -> pure def

readEnv :: (Read a) => String -> a -> IO a
readEnv key def = do
  found <- lookupEnv key
  case found >>= nonEmpty of
    Nothing -> pure def
    Just raw ->
      case readMaybe raw of
        Just value -> pure value
        Nothing -> die $ "Could not parse " <> key <> "=" <> raw

nonEmpty :: String -> Maybe String
nonEmpty raw =
  let value = trim raw
   in if null value then Nothing else Just value

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
