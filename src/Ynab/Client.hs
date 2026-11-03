module Ynab.Client
  ( YnabError (..)
  , ynabErrorText
  , listCategories
  , listUncategorized
  , patchTransactions
  ) where

import Config (Config (..))
import Data.Aeson (FromJSON (..), decode, encode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody (..)
  , host
  , httpLbs
  , method
  , parseRequest
  , path
  , port
  , queryString
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , secure
  , setQueryString
  )
import Network.HTTP.Types (hAuthorization, hContentType, statusCode)
import Ynab.Types
  ( TransactionPatch
  , YnabCategoryGroup
  , YnabTransaction (..)
  )

data YnabError = YnabError
  { ynabStatus :: Int
  , ynabBody :: LBS.ByteString
  }
  deriving (Eq, Show)

ynabErrorText :: YnabError -> Text
ynabErrorText err =
  T.pack ("YNAB HTTP " <> show (ynabStatus err) <> ": ")
    <> T.pack (LBS8.unpack (ynabBody err))

newtype CategoriesEnvelope = CategoriesEnvelope
  { ceGroups :: [YnabCategoryGroup]
  }

instance FromJSON CategoriesEnvelope where
  parseJSON = withObject "CategoriesEnvelope" $ \o -> do
    d <- o .: "data"
    CategoriesEnvelope <$> d .: "category_groups"

newtype TransactionsEnvelope = TransactionsEnvelope
  { teTransactions :: [YnabTransaction]
  }

instance FromJSON TransactionsEnvelope where
  parseJSON = withObject "TransactionsEnvelope" $ \o -> do
    d <- o .: "data"
    TransactionsEnvelope <$> d .: "transactions"

listCategories :: Manager -> Config -> IO (Either YnabError [YnabCategoryGroup])
listCategories manager cfg = do
  req <- parseRequest $ base cfg <> "/categories"
  perform manager (authorize cfg req) $ \body ->
    case decode body of
      Just env -> Right (ceGroups env)
      Nothing -> Left (YnabError 200 body)

listUncategorized :: Manager -> Config -> Maybe Text -> IO (Either YnabError [YnabTransaction])
listUncategorized manager cfg mSince = do
  req0 <- parseRequest $ base cfg <> "/transactions"
  let qs =
        ("type", Just "uncategorized")
          : maybe [] (\d -> [("since_date", Just (encodeUtf8 d))]) mSince
      req = setQueryString qs req0
  perform manager (authorize cfg req) $ \body ->
    case decode body of
      Just env -> Right (filter keep (teTransactions env))
      Nothing -> Left (YnabError 200 body)
  where
    keep tx = not (txDeleted tx)

patchTransactions :: Manager -> Config -> [TransactionPatch] -> IO (Either YnabError ())
patchTransactions _ _ [] = pure (Right ())
patchTransactions manager cfg patches = do
  req0 <- parseRequest $ base cfg <> "/transactions"
  let payload = object ["transactions" .= patches]
      req =
        authorize cfg $
          req0
            { method = "PATCH"
            , requestHeaders =
                (hContentType, "application/json") : requestHeaders req0
            , requestBody = RequestBodyLBS (encode payload)
            }
  perform manager req $ \_ -> Right ()

base :: Config -> String
base cfg =
  "https://api.ynab.com/v1/plans/" <> T.unpack (cfgYnabPlanId cfg)

authorize :: Config -> Request -> Request
authorize cfg req =
  req
    { requestHeaders =
        (hAuthorization, BS8.pack ("Bearer " <> T.unpack (cfgYnabAccessToken cfg)))
          : requestHeaders req
    }

perform :: Manager -> Request -> (LBS.ByteString -> Either YnabError a) -> IO (Either YnabError a)
perform manager req parse = do
  response <- httpLbs req manager
  let code = statusCode (responseStatus response)
      body = responseBody response
  logExchange req code body
  if code >= 200 && code < 300
    then pure (parse body)
    else pure (Left (YnabError code body))

logExchange :: Request -> Int -> LBS.ByteString -> IO ()
logExchange req code body = do
  putStrLn $ "[ynab] " <> BS8.unpack (method req) <> " " <> requestUrl req
  putStrLn "[ynab] request body:"
  putStrLn (renderBody (requestBody req))
  putStrLn $ "[ynab] response " <> show code <> ":"
  putStrLn (LBS8.unpack body)

requestUrl :: Request -> String
requestUrl req =
  let scheme = if secure req then "https://" else "http://"
      hostName = BS8.unpack (host req)
      portNumber = port req
      defaultPort = if secure req then 443 else 80
      portPart = if portNumber == defaultPort then "" else ":" <> show portNumber
   in scheme
        <> hostName
        <> portPart
        <> BS8.unpack (path req)
        <> BS8.unpack (queryString req)

renderBody :: RequestBody -> String
renderBody (RequestBodyLBS bytes)
  | LBS.null bytes = "(empty)"
  | otherwise = LBS8.unpack bytes
renderBody (RequestBodyBS bytes)
  | BS8.null bytes = "(empty)"
  | otherwise = BS8.unpack bytes
renderBody _ = "(body not logged)"
