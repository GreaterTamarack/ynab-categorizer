{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Application
  ( makeApp
  , appMain
  ) where

import App
import Config (Config (..), loadConfig, loadEnvFile)
import qualified Data.Text as T
import Handler.Categories (getCategoriesR)
import Handler.Health (getHealthR)
import Handler.Runs (postRunsR)
import Llm.Claude (mkCategorizer)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Rules (loadRulesFile)
import System.Exit (die)
import System.IO (hFlush, stdout)
import Ynab.Client (listCategories, ynabErrorText)
import Yesod.Core (mkYesodDispatch, toWaiApp)

mkYesodDispatch "App" resourcesApp

makeApp :: IO App
makeApp = do
  loadEnvFile
  cfg <- loadConfig
  manager <- newManager tlsManagerSettings
  groupsResult <- listCategories manager cfg
  groups <-
    case groupsResult of
      Left err -> die (T.unpack (ynabErrorText err))
      Right gs -> pure gs
  rulesResult <- loadRulesFile (cfgRulesFile cfg) groups
  rules <-
    case rulesResult of
      Left err -> die err
      Right r -> pure r
  let categorizer = mkCategorizer manager cfg
  pure
    App
      { appConfig = cfg
      , appManager = manager
      , appRules = rules
      , appCategorizer = categorizer
      }

appMain :: IO ()
appMain = do
  foundation <- makeApp
  wai <- toWaiApp foundation
  let port = cfgPort (appConfig foundation)
      -- Bind loopback only; public exposure is a later phase.
      settings = setHost "127.0.0.1" $ setPort port defaultSettings
  putStrLn $ "Listening on 127.0.0.1:" <> show port
  hFlush stdout
  runSettings settings wai
