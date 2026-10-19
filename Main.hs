{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status404, status204)
import Handler (foo)
import Bar (bar)

data Receipt = Receipt
  { fooid :: String
  , value :: String
  } deriving (Generic)

instance FromJSON Receipt
instance ToJSON Receipt

main :: IO ()
main = do
  store <- newIORef (Map.empty :: Map String String)
  scotty 3000 $ do
    post "/receipts" $ do
      liftIO $ putStrLn foo
      liftIO $ putStrLn (show bar)
      receipt <- jsonData :: ActionM Receipt
      liftIO $ atomicModifyIORef' store $ \receipts ->
        (Map.insert (fooid receipt) (value receipt) receipts, ())
      json receipt

    get "/receipts/:fooid" $ do
      receiptId <- pathParam "fooid"
      receipts <- liftIO $ readIORef store
      case Map.lookup receiptId receipts of
        Nothing -> do
          status status404
          text "not found"
        Just receiptValue -> json (Receipt receiptId receiptValue)

    delete "/receipts/:fooid" $ do
      receiptId <- pathParam "fooid"
      liftIO $ atomicModifyIORef' store $ \receipts ->
        (Map.delete receiptId receipts, ())
      status status204
