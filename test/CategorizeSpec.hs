module CategorizeSpec (spec) where

import Categorize
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Llm.Categorizer (Suggestion (..))
import Test.Hspec
import Ynab.Types

spec :: Spec
spec = do
  describe "decideOne" $ do
    it "sets the LLM category and the confident flag above the threshold" $ do
      let d = decideOne settings cats False tx (Just (sug "cat-groc" 0.93 "bulk food"))
      decisionKind d `shouldBe` KindConfident
      tpCategoryId (decisionPatch d) `shouldBe` Just "cat-groc"
      tpFlagColor (decisionPatch d) `shouldBe` FlagGreen
      decisionChosenName d `shouldBe` "Groceries"
      T.unpack (tpMemo (decisionPatch d)) `shouldStartWith` "[auto 0.93 confident] bulk food || "
      patchHasApproved (decisionPatch d) `shouldBe` False

    it "keeps an existing category below the threshold and sets the review flag" $ do
      let existing = tx {txCategoryId = Just "cat-groc", txCategoryName = Just "Groceries"}
          d = decideOne settings cats False existing (Just (sug "cat-din" 0.40 "maybe dining"))
      decisionKind d `shouldBe` KindNeedsReview
      tpCategoryId (decisionPatch d) `shouldBe` Just "cat-groc"
      tpFlagColor (decisionPatch d) `shouldBe` FlagYellow
      T.unpack (tpMemo (decisionPatch d)) `shouldStartWith` "[auto 0.40 review: Dining] maybe dining || "
      patchHasApproved (decisionPatch d) `shouldBe` False

    it "applies the LLM category below the threshold when none exists" $ do
      let d = decideOne settings cats False tx (Just (sug "cat-din" 0.40 "maybe dining"))
      decisionKind d `shouldBe` KindNeedsReview
      tpCategoryId (decisionPatch d) `shouldBe` Just "cat-din"
      tpFlagColor (decisionPatch d) `shouldBe` FlagYellow

    it "does not set a category on split transactions" $ do
      let splitTx =
            tx
              { txCategoryName = Just "Split"
              , txSubtransactions = [objectEmpty]
              }
          d = decideOne settings cats False splitTx (Just (sug "cat-groc" 0.99 "ignore me"))
      decisionKind d `shouldBe` KindUnsupported
      tpCategoryId (decisionPatch d) `shouldBe` Nothing
      tpFlagColor (decisionPatch d) `shouldBe` FlagYellow
      patchHasApproved (decisionPatch d) `shouldBe` False

    it "does not set a category on transfers" $ do
      let transferTx = tx {txTransferAccountId = Just "other-acc"}
          d = decideOne settings cats False transferTx (Just (sug "cat-groc" 0.99 "ignore me"))
      decisionKind d `shouldBe` KindUnsupported
      tpCategoryId (decisionPatch d) `shouldBe` Nothing

    it "treats an invalid category id as review" $ do
      let d = decideOne settings cats False tx (Just (sug "nope" 0.99 "made it up"))
      decisionKind d `shouldBe` KindNeedsReview
      tpCategoryId (decisionPatch d) `shouldBe` Nothing
      T.unpack (tpMemo (decisionPatch d)) `shouldStartWith` "[auto 0.99 review] invalid category id || "

    it "treats a missing suggestion as review" $ do
      let d = decideOne settings cats False tx Nothing
      decisionKind d `shouldBe` KindNeedsReview
      T.unpack (tpMemo (decisionPatch d)) `shouldStartWith` "[auto n/a review] no model suggestion || "

  describe "memo tagging" $ do
    it "strips an existing auto tag so re-runs do not stack" $ do
      let first = buildMemo "[auto 0.90 confident] first || " "coffee"
          second = buildMemo "[auto 0.80 confident] second || " first
      second `shouldBe` "[auto 0.80 confident] second || coffee"

    it "is a no-op strip when no tag is present" $ do
      stripAutoTag "just a memo" `shouldBe` "just a memo"

    it "truncates to 500 characters by cutting the original memo" $ do
      let original = T.replicate 600 "x"
          tag = "[auto 0.90 confident] reason || "
          memo = buildMemo tag original
      T.length memo `shouldBe` 500
      T.take (T.length tag) memo `shouldBe` tag

settings :: DecisionSettings
settings =
  DecisionSettings
    { dsThreshold = 0.8
    , dsConfidentFlag = FlagGreen
    , dsReviewFlag = FlagYellow
    }

cats :: CategoryLookup
cats =
  CategoryLookup
    { clNameOf = \cid -> lookup cid [("cat-groc", "Groceries"), ("cat-din", "Dining")]
    , clValid = \cid -> cid `elem` ["cat-groc", "cat-din"]
    }

tx :: YnabTransaction
tx =
  YnabTransaction
    { txId = "tx1"
    , txDate = "2026-11-01"
    , txAmount = -84230
    , txMemo = Nothing
    , txApproved = False
    , txFlagColor = Nothing
    , txAccountId = "acc"
    , txAccountName = "Chase"
    , txPayeeName = Just "COSTCO #123"
    , txCategoryId = Nothing
    , txCategoryName = Nothing
    , txTransferAccountId = Nothing
    , txDeleted = False
    , txSubtransactions = []
    }

sug :: CategoryId -> Double -> Text -> Suggestion
sug cid conf reason =
  Suggestion
    { sTransactionId = "tx1"
    , sCategoryId = cid
    , sConfidence = conf
    , sReason = reason
    }

patchHasApproved :: TransactionPatch -> Bool
patchHasApproved patch =
  case decode (encode patch) of
    Just (Object o) -> KM.member "approved" o
    _ -> True

objectEmpty :: Value
objectEmpty = Object mempty
