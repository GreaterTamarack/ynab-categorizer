module Run
  ( RunRequest (..)
  , RunError (..)
  , RunReport (..)
  , RunCounts (..)
  , DecisionReport (..)
  , Snapshot (..)
  , executeRun
  ) where

import Categorize
  ( CategoryLookup (..)
  , Decision (..)
  , DecisionKind (..)
  , DecisionSettings (..)
  , decideOne
  )
import Config (Config (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Llm.Categorizer
  ( CategorizeRequest (..)
  , Categorizer (..)
  , LlmError (..)
  , RuleHint (..)
  , Suggestion (..)
  , TxForLlm (..)
  )
import Network.HTTP.Client (Manager)
import Rules
  ( CandidateCategory (..)
  , ResolvedRule (..)
  , Rules (..)
  , candidateCategories
  , matchRule
  )
import Ynab.Client (YnabError, listCategories, listUnapproved, patchTransactions, ynabErrorText)
import Ynab.Types
  ( CategoryId
  , FlagColor
  , TransactionId
  , TransactionPatch (..)
  , YnabCategoryGroup
  , YnabTransaction (..)
  , categoryLabel
  , flagColorToText
  , hasRealCategory
  , payeeLabel
  )

data RunRequest = RunRequest
  { rrApply :: Bool
  , rrSinceDate :: Maybe Text
  }

data RunError
  = RunYnab YnabError
  | RunLlm LlmError
  | RunMessage Text
  deriving (Show)

data Snapshot = Snapshot
  { snapCategory :: Text
  , snapMemo :: Maybe Text
  , snapFlag :: Maybe FlagColor
  }

data DecisionReport = DecisionReport
  { drTransactionId :: TransactionId
  , drDate :: Text
  , drPayee :: Text
  , drAmountMilliunits :: Integer
  , drBefore :: Snapshot
  , drAfter :: Snapshot
  , drConfidence :: Maybe Double
  , drReason :: Maybe Text
  , drRuleMatched :: Bool
  }

data RunCounts = RunCounts
  { countConfident :: Int
  , countNeedsReview :: Int
  , countUnsupported :: Int
  }

data RunReport = RunReport
  { reportMode :: Text
  , reportThreshold :: Double
  , reportCounts :: RunCounts
  , reportDecisions :: [DecisionReport]
  }

instance ToJSON Snapshot where
  toJSON s =
    object
      [ "category" .= snapCategory s
      , "memo" .= snapMemo s
      , "flag" .= fmap flagColorToText (snapFlag s)
      ]

instance ToJSON DecisionReport where
  toJSON d =
    object
      [ "transactionId" .= drTransactionId d
      , "date" .= drDate d
      , "payee" .= drPayee d
      , "amountMilliunits" .= drAmountMilliunits d
      , "before" .= drBefore d
      , "after" .= drAfter d
      , "confidence" .= drConfidence d
      , "reason" .= drReason d
      , "ruleMatched" .= drRuleMatched d
      ]

instance ToJSON RunCounts where
  toJSON c =
    object
      [ "confident" .= countConfident c
      , "needsReview" .= countNeedsReview c
      , "unsupported" .= countUnsupported c
      ]

instance ToJSON RunReport where
  toJSON r =
    object
      [ "mode" .= reportMode r
      , "threshold" .= reportThreshold r
      , "counts" .= reportCounts r
      , "decisions" .= reportDecisions r
      ]

instance ToJSON RunError where
  toJSON (RunYnab err) = object ["error" .= ynabErrorText err]
  toJSON (RunLlm err) =
    object
      [ "error" .= llmBody err
      , "status" .= llmStatus err
      ]
  toJSON (RunMessage msg) = object ["error" .= msg]

executeRun :: Manager -> Config -> Rules -> Categorizer -> RunRequest -> IO (Either RunError RunReport)
executeRun manager cfg rules categorizer request = do
  groupsResult <- listCategories manager cfg
  case groupsResult of
    Left err -> pure (Left (RunYnab err))
    Right groups -> do
      txResult <- listUnapproved manager cfg (rrSinceDate request)
      case txResult of
        Left err -> pure (Left (RunYnab err))
        Right txs -> complete groups txs
  where
    complete :: [YnabCategoryGroup] -> [YnabTransaction] -> IO (Either RunError RunReport)
    complete groups txs = do
      let candidates = candidateCategories rules groups
          lookupMap = indexCandidates candidates
          cats =
            CategoryLookup
              { clNameOf = \cid -> fmap ccName (Map.lookup cid lookupMap)
              , clValid = \cid -> Map.member cid lookupMap
              }
          settings =
            DecisionSettings
              { dsThreshold = cfgConfidenceThreshold cfg
              , dsConfidentFlag = cfgConfidentFlagColor cfg
              , dsReviewFlag = cfgReviewFlagColor cfg
              }
          prepared = map (prepare rules) txs
      if null candidates
        then pure (Left (RunMessage "No candidate categories after filtering."))
        else do
          llmResult <-
            categorize
              categorizer
              CategorizeRequest
                { crCategories = candidates
                , crHints = rulesHints rules
                , crTransactions = map fst prepared
                }
          case llmResult of
            Left err -> pure (Left (RunLlm err))
            Right suggestions -> applyDecisions settings cats prepared suggestions

    applyDecisions settings cats prepared suggestions = do
      let byId = Map.fromList [(sTransactionId s, s) | s <- suggestions]
          decisions =
            [ decideOne settings cats matched tx (Map.lookup (txId tx) byId)
            | (_, (tx, matched)) <- prepared
            ]
          report = buildReport cfg request decisions
      if rrApply request
        then do
          patchResult <- patchTransactions manager cfg (map decisionPatch decisions)
          case patchResult of
            Left err -> pure (Left (RunYnab err))
            Right () -> pure (Right report)
        else pure (Right report)

prepare :: Rules -> YnabTransaction -> (TxForLlm, (YnabTransaction, Bool))
prepare rules tx =
  let matched = matchRule rules (payeeLabel tx) (txAmount tx)
      hint =
        fmap
          ( \rule ->
              RuleHint
                { rhCategoryName = rrCategoryName rule
                , rhNote = rrNote rule
                }
          )
          matched
      item =
        TxForLlm
          { tlId = txId tx
          , tlDate = txDate tx
          , tlPayee = payeeLabel tx
          , tlAmountMilliunits = txAmount tx
          , tlAccountName = txAccountName tx
          , tlExistingCategory =
              if hasRealCategory tx then txCategoryName tx else Nothing
          , tlMemo = txMemo tx
          , tlSuggestedRule = hint
          }
   in (item, (tx, maybe False (const True) matched))

indexCandidates :: [CandidateCategory] -> Map CategoryId CandidateCategory
indexCandidates = Map.fromList . map (\c -> (ccId c, c))

buildReport :: Config -> RunRequest -> [Decision] -> RunReport
buildReport cfg request decisions =
  RunReport
    { reportMode = if rrApply request then "apply" else "dry-run"
    , reportThreshold = cfgConfidenceThreshold cfg
    , reportCounts =
        RunCounts
          { countConfident = counted KindConfident
          , countNeedsReview = counted KindNeedsReview
          , countUnsupported = counted KindUnsupported
          }
    , reportDecisions = map toReport decisions
    }
  where
    counted kind = length (filter ((== kind) . decisionKind) decisions)

toReport :: Decision -> DecisionReport
toReport d =
  let tx = decisionTx d
      patch = decisionPatch d
   in DecisionReport
        { drTransactionId = txId tx
        , drDate = txDate tx
        , drPayee = payeeLabel tx
        , drAmountMilliunits = txAmount tx
        , drBefore =
            Snapshot
              { snapCategory = categoryLabel tx
              , snapMemo = txMemo tx
              , snapFlag = txFlagColor tx
              }
        , drAfter =
            Snapshot
              { snapCategory = decisionChosenName d
              , snapMemo = Just (tpMemo patch)
              , snapFlag = Just (tpFlagColor patch)
              }
        , drConfidence = decisionConfidence d
        , drReason = decisionReason d
        , drRuleMatched = decisionRuleMatched d
        }
