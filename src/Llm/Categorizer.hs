module Llm.Categorizer
  ( Categorizer (..)
  , CategorizeRequest (..)
  , TxForLlm (..)
  , RuleHint (..)
  , Suggestion (..)
  , LlmError (..)
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import Rules (CandidateCategory)
import Ynab.Types (CategoryId, TransactionId)

data LlmError = LlmError
  { llmStatus :: Maybe Int
  , llmBody :: Text
  }
  deriving (Eq, Show)

data RuleHint = RuleHint
  { rhCategoryName :: Text
  , rhNote :: Maybe Text
  }
  deriving (Eq, Show)

data TxForLlm = TxForLlm
  { tlId :: TransactionId
  , tlDate :: Text
  , tlPayee :: Text
  , tlAmountMilliunits :: Integer
  , tlAccountName :: Text
  , tlExistingCategory :: Maybe Text
  , tlMemo :: Maybe Text
  , tlSuggestedRule :: Maybe RuleHint
  }
  deriving (Eq, Show)

data CategorizeRequest = CategorizeRequest
  { crCategories :: [CandidateCategory]
  , crHints :: [Text]
  , crTransactions :: [TxForLlm]
  }

data Suggestion = Suggestion
  { sTransactionId :: TransactionId
  , sCategoryId :: CategoryId
  , sConfidence :: Double
  , sReason :: Text
  }
  deriving (Eq, Show)

instance FromJSON Suggestion where
  parseJSON = withObject "Suggestion" $ \o ->
    Suggestion
      <$> o .: "transactionId"
      <*> o .: "categoryId"
      <*> o .: "confidence"
      <*> o .: "reason"

newtype Categorizer = Categorizer
  { categorize :: CategorizeRequest -> IO (Either LlmError [Suggestion])
  }
