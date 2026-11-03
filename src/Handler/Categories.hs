module Handler.Categories
  ( getCategoriesR
  ) where

import App
import Network.HTTP.Types (status502)
import Rules (candidateCategories)
import Ynab.Client (listCategories, ynabErrorText)
import Yesod.Core

getCategoriesR :: Handler Value
getCategoriesR = do
  App {appConfig = cfg, appManager = manager, appRules = rules} <- getYesod
  result <- liftIO $ listCategories manager cfg
  case result of
    Left err -> sendStatusJSON status502 $ object ["error" .= ynabErrorText err]
    Right groups ->
      returnJson $ object ["categories" .= candidateCategories rules groups]
