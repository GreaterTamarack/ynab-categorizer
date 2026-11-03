module Ynab.Types
  ( CategoryId
  , TransactionId
  , FlagColor (..)
  , parseFlagColor
  , flagColorToText
  , YnabCategory (..)
  , YnabCategoryGroup (..)
  , YnabTransaction (..)
  , TransactionPatch (..)
  , isSplit
  , isTransfer
  , isUnsupported
  , hasRealCategory
  , categoryLabel
  , payeeLabel
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , object
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

type CategoryId = Text
type TransactionId = Text

data FlagColor
  = FlagRed
  | FlagOrange
  | FlagYellow
  | FlagGreen
  | FlagBlue
  | FlagPurple
  deriving (Eq, Show)

parseFlagColor :: String -> Maybe FlagColor
parseFlagColor raw =
  case fmap toLower raw of
    "red" -> Just FlagRed
    "orange" -> Just FlagOrange
    "yellow" -> Just FlagYellow
    "green" -> Just FlagGreen
    "blue" -> Just FlagBlue
    "purple" -> Just FlagPurple
    _ -> Nothing

flagColorToText :: FlagColor -> Text
flagColorToText FlagRed = "red"
flagColorToText FlagOrange = "orange"
flagColorToText FlagYellow = "yellow"
flagColorToText FlagGreen = "green"
flagColorToText FlagBlue = "blue"
flagColorToText FlagPurple = "purple"

instance ToJSON FlagColor where
  toJSON = String . flagColorToText

instance FromJSON FlagColor where
  parseJSON = withText "FlagColor" $ \t ->
    case parseFlagColor (T.unpack t) of
      Just color -> pure color
      Nothing -> fail $ "unknown flag color: " <> T.unpack t

data YnabCategory = YnabCategory
  { catId :: CategoryId
  , catGroupId :: Text
  , catName :: Text
  , catHidden :: Bool
  , catInternal :: Bool
  , catDeleted :: Bool
  }
  deriving (Eq, Show)

instance FromJSON YnabCategory where
  parseJSON = withObject "Category" $ \o ->
    YnabCategory
      <$> o .: "id"
      <*> o .: "category_group_id"
      <*> o .: "name"
      <*> o .:? "hidden" .!= False
      <*> o .:? "internal" .!= False
      <*> o .:? "deleted" .!= False

data YnabCategoryGroup = YnabCategoryGroup
  { cgId :: Text
  , cgName :: Text
  , cgHidden :: Bool
  , cgInternal :: Bool
  , cgDeleted :: Bool
  , cgCategories :: [YnabCategory]
  }
  deriving (Eq, Show)

instance FromJSON YnabCategoryGroup where
  parseJSON = withObject "CategoryGroup" $ \o ->
    YnabCategoryGroup
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "hidden" .!= False
      <*> o .:? "internal" .!= False
      <*> o .:? "deleted" .!= False
      <*> o .:? "categories" .!= []

data YnabTransaction = YnabTransaction
  { txId :: TransactionId
  , txDate :: Text
  , txAmount :: Integer
  , txMemo :: Maybe Text
  , txApproved :: Bool
  , txFlagColor :: Maybe FlagColor
  , txAccountId :: Text
  , txAccountName :: Text
  , txPayeeName :: Maybe Text
  , txCategoryId :: Maybe CategoryId
  , txCategoryName :: Maybe Text
  , txTransferAccountId :: Maybe Text
  , txDeleted :: Bool
  , txSubtransactions :: [Value]
  }
  deriving (Eq, Show)

instance FromJSON YnabTransaction where
  parseJSON = withObject "Transaction" $ \o -> do
    mFlagText <- o .:? "flag_color"
    YnabTransaction
      <$> o .: "id"
      <*> o .: "date"
      <*> o .: "amount"
      <*> o .:? "memo"
      <*> o .:? "approved" .!= False
      <*> pure (parseTxFlag mFlagText)
      <*> o .: "account_id"
      <*> o .:? "account_name" .!= ""
      <*> o .:? "payee_name"
      <*> o .:? "category_id"
      <*> o .:? "category_name"
      <*> o .:? "transfer_account_id"
      <*> o .:? "deleted" .!= False
      <*> o .:? "subtransactions" .!= []

parseTxFlag :: Maybe Text -> Maybe FlagColor
parseTxFlag Nothing = Nothing
parseTxFlag (Just t)
  | T.null t = Nothing
  | otherwise = parseFlagColor (T.unpack t)

data TransactionPatch = TransactionPatch
  { tpId :: TransactionId
  , tpCategoryId :: Maybe CategoryId
  , tpMemo :: Text
  , tpFlagColor :: FlagColor
  }
  deriving (Eq, Show)

instance ToJSON TransactionPatch where
  toJSON p =
    object $
      [ "id" .= tpId p
      , "memo" .= tpMemo p
      , "flag_color" .= tpFlagColor p
      ]
        ++ ["category_id" .= cid | cid <- maybeToList (tpCategoryId p)]

isSplit :: YnabTransaction -> Bool
isSplit tx =
  not (null (txSubtransactions tx)) || txCategoryName tx == Just "Split"

isTransfer :: YnabTransaction -> Bool
isTransfer = isJust . txTransferAccountId

isUnsupported :: YnabTransaction -> Bool
isUnsupported tx = isSplit tx || isTransfer tx

hasRealCategory :: YnabTransaction -> Bool
hasRealCategory tx =
  case txCategoryId tx of
    Nothing -> False
    Just _ ->
      case txCategoryName tx of
        Just "Uncategorized" -> False
        Just "Split" -> False
        _ -> True

categoryLabel :: YnabTransaction -> Text
categoryLabel tx = fromMaybe "Uncategorized" (txCategoryName tx)

payeeLabel :: YnabTransaction -> Text
payeeLabel tx = fromMaybe "" (txPayeeName tx)
