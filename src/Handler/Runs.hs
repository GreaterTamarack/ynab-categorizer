module Handler.Runs
  ( postRunsR
  ) where

import App
import Config (Config (..))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status400, status401, status502)
import Run (RunRequest (..), executeRun)
import qualified Data.Text as T
import Yesod.Core

postRunsR :: Handler Value
postRunsR = do
  requireApiKey
  App
    { appConfig = cfg
    , appManager = manager
    , appRules = rules
    , appCategorizer = categorizer
    } <-
    getYesod
  apply <- fmap (== Just "true") (lookupGetParam "apply")
  since <- lookupGetParam "sinceDate"
  case since of
    Just d | not (validDate d) ->
      sendStatusJSON status400 $
        object ["error" .= ("sinceDate must be YYYY-MM-DD" :: T.Text)]
    _ -> do
      result <-
        liftIO $
          executeRun
            manager
            cfg
            rules
            categorizer
            RunRequest {rrApply = apply, rrSinceDate = since}
      case result of
        Left err -> sendStatusJSON status502 err
        Right report -> returnJson report

requireApiKey :: Handler ()
requireApiKey = do
  App {appConfig = cfg} <- getYesod
  provided <- lookupHeader "X-Api-Key"
  if provided == Just (encodeUtf8 (cfgApiKey cfg))
    then pure ()
    else
      sendStatusJSON status401 $
        object ["error" .= ("unauthorized" :: T.Text)]

validDate :: T.Text -> Bool
validDate t =
  T.length t == 10
    && T.index t 4 == '-'
    && T.index t 7 == '-'
