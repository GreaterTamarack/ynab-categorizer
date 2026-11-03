module Ynab.Client
  ( YnabError (..)
  , ynabErrorText
  , listCategories
  , listUnapproved
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
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
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

listUnapproved :: Manager -> Config -> Maybe Text -> IO (Either YnabError [YnabTransaction])
listUnapproved manager cfg mSince = do
  req0 <- parseRequest $ base cfg <> "/transactions"
  let qs =
        ("type", Just "unapproved")
          : maybe [] (\d -> [("since_date", Just (encodeUtf8 d))]) mSince
      req = setQueryString qs req0
  perform manager (authorize cfg req) $ \body ->
    case decode body of
      Just env -> Right (filter keep (teTransactions env))
      Nothing -> Left (YnabError 200 body)
  where
    keep tx = not (txDeleted tx) && not (txApproved tx)

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
  if code >= 200 && code < 300
    then pure (parse body)
    else pure (Left (YnabError code body))
