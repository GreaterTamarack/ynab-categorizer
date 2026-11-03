{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module App
  ( App (..)
  , Route (..)
  , resourcesApp
  , Handler
  , Widget
  ) where

import Config (Config)
import Data.Text (Text)
import qualified Data.Text as T
import Llm.Categorizer (Categorizer)
import Network.HTTP.Client (Manager)
import Rules (Rules)
import Yesod.Core

data App = App
  { appConfig :: Config
  , appManager :: Manager
  , appRules :: Rules
  , appCategorizer :: Categorizer
  }

mkYesodData "App" [parseRoutes|
/health HealthR GET
/categories CategoriesR GET
/runs RunsR POST
|]

instance Yesod App where
  makeSessionBackend _ = pure Nothing
  defaultLayout _ = pure mempty
  errorHandler err =
    selectRep $ provideJson $ object ["error" .= errorMessage err]

errorMessage :: ErrorResponse -> Text
errorMessage NotFound = "not found"
errorMessage (InternalError t) = t
errorMessage (InvalidArgs xs) = T.intercalate ", " xs
errorMessage NotAuthenticated = "not authenticated"
errorMessage (PermissionDenied t) = t
errorMessage (BadMethod _) = "bad method"
