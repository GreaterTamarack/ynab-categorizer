{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isSpace)
import Network.HTTP.Client (httpLbs, newManager, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import System.Exit (die)

plansUrl :: String
plansUrl = "https://api.ynab.com/v1/plans"

main :: IO ()
main = do
  body <- fetchPlans
  LBS.putStr body
  LBS.putStr "\n"

-- GET /v1/plans. A real environment variable wins over .env.
fetchPlans :: IO LBS.ByteString
fetchPlans = do
  onMissingFile (loadFile defaultConfig) (pure ())
  token <- requireAccessToken
  manager <- newManager tlsManagerSettings
  request <- parseRequest plansUrl
  let authorized =
        request
          { requestHeaders =
              (hAuthorization, BS.pack ("Bearer " ++ token)) : requestHeaders request
          }
  response <- httpLbs authorized manager
  let code = statusCode (responseStatus response)
      body = responseBody response
  if code == 200
    then pure body
    else die $ "YNAB returned HTTP " ++ show code ++ "\n" ++ LBS8.unpack body

requireAccessToken :: IO String
requireAccessToken = do
  found <- lookupEnv "YNAB_ACCESS_TOKEN"
  case found >>= nonEmpty of
    Just token -> pure token
    Nothing -> die "Set YNAB_ACCESS_TOKEN in .env or the environment."

nonEmpty :: String -> Maybe String
nonEmpty raw =
  let token = trim raw
   in if null token then Nothing else Just token

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
