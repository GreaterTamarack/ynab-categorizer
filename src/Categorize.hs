module Categorize
  ( DecisionKind (..)
  , DecisionSettings (..)
  , CategoryLookup (..)
  , Decision (..)
  , decideOne
  , stripAutoTag
  , buildMemo
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Llm.Categorizer (Suggestion (..))
import Numeric (showFFloat)
import Ynab.Types
  ( CategoryId
  , FlagColor
  , TransactionPatch (..)
  , YnabTransaction (..)
  , categoryLabel
  , hasRealCategory
  , isUnsupported
  )

data DecisionKind
  = KindConfident
  | KindNeedsReview
  | KindUnsupported
  deriving (Eq, Show)

data DecisionSettings = DecisionSettings
  { dsThreshold :: Double
  , dsConfidentFlag :: FlagColor
  , dsReviewFlag :: FlagColor
  }

data CategoryLookup = CategoryLookup
  { clNameOf :: CategoryId -> Maybe Text
  , clValid :: CategoryId -> Bool
  }

data Decision = Decision
  { decisionKind :: DecisionKind
  , decisionTx :: YnabTransaction
  , decisionSuggestedName :: Maybe Text
  , decisionChosenId :: Maybe CategoryId
  , decisionChosenName :: Text
  , decisionConfidence :: Maybe Double
  , decisionReason :: Maybe Text
  , decisionRuleMatched :: Bool
  , decisionPatch :: TransactionPatch
  }

-- =============================================================================
-- decideOne: PURPOSE
--   Given one YNAB transaction and (maybe) an LLM category suggestion for it,
--   decide what to do: auto-apply the category ("confident"), flag it for a
--   human ("needs review"), or give up ("unsupported", e.g. splits/transfers).
--   The result is a `Decision` record, which includes the `TransactionPatch`
--   that will later be sent to the YNAB API.
--
-- HASKELL SYNTAX PRIMER FOR THIS SECTION
--   `--`            starts a line comment (like `//` in C/JS).
--   `name :: Type`  is a TYPE SIGNATURE. Read `::` as "has type".
--   `A -> B`        (the "arrow") is the type of a FUNCTION from A to B.
--                   Arrows chain: `A -> B -> C` means "takes an A, then a B,
--                   returns a C". The LAST type is always the return type;
--                   everything before it is a parameter type.
--   `f x y`         is FUNCTION APPLICATION. Haskell has no `f(x, y)`; you
--                   write the function, then its arguments separated by
--                   spaces. Parentheses are only for grouping, e.g.
--                   `f (g x)` passes the result of `g x` as one argument.
--   `Maybe T`       is a type meaning "either a T, or nothing". Its two values
--                   are `Just x` (there is a value x) and `Nothing` (there
--                   isn't). This is Haskell's replacement for null.
--                   `Just` and `Nothing` are not keywords; they are
--                   DATA CONSTRUCTORS defined in the standard library as
--                   `data Maybe a = Nothing | Just a`.
--   Capitalised vs lowercase names:
--     Capitalised (`Decision`, `Just`, `KindConfident`) are TYPES or DATA
--     CONSTRUCTORS. Lowercase (`settings`, `tx`, `finish`) are VARIABLES or
--     FUNCTIONS. This is enforced by the compiler, not just a convention.
--   Keywords used below: `case`, `of`, `where`, `let`, `in`.
--   `g` looks like a keyword but is just a library constant equal to
--   `True`, used as a catch-all guard.
--
-- LISTS vs SINGLE VALUES
--   There are NO lists in this section. A Haskell list is written with square
--   brackets, `[a, b, c]`, and has type `[T]`. Everything here is a single
--   value. The things that may look like lists are:
--     * the arguments to `finish`, stacked one per line: that's just ordinary
--       function application split across lines (7 separate arguments).
--     * `{ field = value, ... }`: RECORD syntax, building one struct-like
--       value with named fields (comma-separated, but not a list).
-- =============================================================================

-- TYPE SIGNATURE for the function `decideOne`.
-- It takes 5 parameters (the types before the last `->`) and returns a
-- `Decision` (the type after the last `->`):
--   1. DecisionSettings  -- threshold and flag colours (record defined above)
--   2. CategoryLookup    -- functions to look up / validate category ids
--   3. Bool              -- did a vendor rule match this transaction?
--   4. YnabTransaction   -- the transaction being categorised
--   5. Maybe Suggestion  -- the LLM's suggestion, which might be missing
decideOne :: DecisionSettings -> CategoryLookup -> Bool -> YnabTransaction -> Maybe Suggestion -> Decision
-- FUNCTION DEFINITION. `decideOne` is the function name; the 5 lowercase words
-- after it are PARAMETER NAMES that are bound, in order, to the 5 parameter
-- types above. They're names chosen by the author, not keywords:
--   settings    :: DecisionSettings
--   cats        :: CategoryLookup
--   ruleMatched :: Bool
--   tx          :: YnabTransaction
--   mSug        :: Maybe Suggestion   (the "m" prefix is a common naming
--                                      convention for "Maybe")
-- There's no `=` right after the parameters because the body is written with
-- GUARDS (the `|` lines below).
decideOne settings cats ruleMatched tx mSug
  -- GUARDS: each `| condition = result` is tried top to bottom; the first
  -- condition that is True picks the result. Read `|` as "such that" / "when".
  --
  -- `isUnsupported` is NOT a new variable. It's an existing FUNCTION imported
  -- from Ynab.Types with type `YnabTransaction -> Bool`. `isUnsupported tx`
  -- calls it on `tx` and is True for split or transfer transactions.
  --
  -- SUBSECTION 1: unsupported transactions (splits/transfers). YNAB won't let
  -- us set a category on these, so we leave the category alone and just flag
  -- the transaction for review.
  | isUnsupported tx =
      -- This calls the helper `finish` (defined in the `where` block at the
      -- bottom) with 7 arguments, one per line. It is the same as writing:
      --   finish KindUnsupported Nothing Nothing Nothing (Just "...") (...) Nothing
      -- Each argument lines up with a parameter of `finish`:
      --   kind, chosenId, mS, suggestedIdForName, mReason, flag, patchCategory
      finish
        KindUnsupported               -- kind:               a DecisionKind constructor
        Nothing                       -- chosenId:           no chosen category
        Nothing                       -- mS:                 no suggestion
        Nothing                       -- suggestedIdForName: no suggested category
        (Just "cannot set category")  -- mReason:            a Maybe Text containing the reason
        (dsReviewFlag settings)       -- flag:               read the `dsReviewFlag` field of `settings`
        Nothing                       -- patchCategory:      don't change the category in YNAB
  -- `otherwise` (= True) is the catch-all guard: for every other transaction.
  | otherwise =
      -- `case EXPR of` is PATTERN MATCHING: look at the shape of `mSug` and
      -- run the first branch whose pattern fits. In a `case`, `->` means
      -- "if it matches this pattern, the result is ...". (Same symbol as in
      -- types, different meaning depending on where it appears.)
      case mSug of
        -- SUBSECTION 2: the LLM gave no suggestion. Keep whatever category the
        -- transaction already has and flag it for review.
        -- The pattern `Nothing` matches when mSug has no value.
        Nothing ->
          finish
            KindNeedsReview
            (txCategoryId tx)  -- `txCategoryId` is a record field accessor (a function
                               -- of type YnabTransaction -> Maybe CategoryId), so this
                               -- is already a Maybe and doesn't need `Just`.
            Nothing
            Nothing
            (Just "no model suggestion")
            (dsReviewFlag settings)
            (txCategoryId tx)  -- patch with the existing category, i.e. no change
        -- The pattern `Just sug` matches when mSug has a value, and binds that
        -- value to the NEW variable `sug` (type: Suggestion). `sug` is only in
        -- scope inside this branch.
        --
        -- The `|` lines below are guards again, this time attached to the
        -- `Just sug` case branch. With guards inside a `case`, the result is
        -- written with `->` instead of `=`.
        Just sug
          -- SUBSECTION 3: the LLM suggested a category id that doesn't exist
          -- in the budget (e.g. a hallucination). Ignore it, keep the current
          -- category, and flag for review.
          --   `sCategoryId sug`             gets the suggestion's category id field
          --   `clValid cats (...)`          calls the `clValid` function stored in the
          --                                 `cats` record, giving a Bool
          --   `not (...)`                   is boolean negation (a regular function)
          | not (clValid cats (sCategoryId sug)) ->
              finish
                KindNeedsReview
                (txCategoryId tx)
                (Just sug)              -- wrap `sug` in Just because `finish` expects a Maybe Suggestion
                Nothing
                (Just "invalid category id")
                (dsReviewFlag settings)
                (txCategoryId tx)
          -- SUBSECTION 4: the suggestion is valid and its confidence meets
          -- the threshold. Apply the suggested category and use the
          -- "confident" flag colour.
          --   `>=` is the "greater than or equal to" operator.
          --   Function application binds tighter than any operator, so this
          --   parses as `(sConfidence sug) >= (dsThreshold settings)`.
          | sConfidence sug >= dsThreshold settings ->
              finish
                KindConfident
                (Just (sCategoryId sug))  -- nested parentheses: compute `sCategoryId sug`
                                          -- first, then wrap it in `Just`
                (Just sug)
                (Just (sCategoryId sug))
                (Just (sReason sug))
                (dsConfidentFlag settings)
                (Just (sCategoryId sug))  -- patch YNAB with the suggested category
          -- SUBSECTION 5: low confidence, and the transaction already has a
          -- real category. Don't overwrite it; record the suggestion in the
          -- memo and flag for review.
          -- `hasRealCategory` is another imported function, YnabTransaction -> Bool.
          | hasRealCategory tx ->
              finish
                KindNeedsReview
                (txCategoryId tx)
                (Just sug)
                (Just (sCategoryId sug))
                (Just (sReason sug))
                (dsReviewFlag settings)
                (txCategoryId tx)         -- keep the existing category
          -- SUBSECTION 6: low confidence and the transaction is uncategorised.
          -- A low-confidence guess beats nothing, so apply it, but flag it for
          -- review.
          | otherwise ->
              finish
                KindNeedsReview
                (Just (sCategoryId sug))
                (Just sug)
                (Just (sCategoryId sug))
                (Just (sReason sug))
                (dsReviewFlag settings)
                (Just (sCategoryId sug))  -- apply the suggestion tentatively
  -- `where` is a KEYWORD that adds local definitions to the function above
  -- it. Anything defined here is visible throughout `decideOne`'s body (that's
  -- why `finish` can be used above before it's defined), and it can also use
  -- `decideOne`'s parameters such as `cats`, `tx` and `ruleMatched`.
  -- Order doesn't matter in Haskell: definitions can come after they're used.
  where
    -- SUBSECTION 7: `finish` is a LOCAL HELPER FUNCTION shared by every
    -- branch above. It turns the 7 inputs into the final `Decision`, including
    -- the memo tag and the YNAB patch.
    --
    -- Syntax: `name param1 param2 ... = body`. This is the same shape as
    -- `decideOne`'s definition, but with a plain `=` and no guards. There's no
    -- type signature here, so the compiler INFERS the parameter types from
    -- how they are used:
    --   kind               :: DecisionKind
    --   chosenId           :: Maybe CategoryId  (category to show as "chosen")
    --   mS                 :: Maybe Suggestion
    --   suggestedIdForName :: Maybe CategoryId  (category to name in the memo tag)
    --   mReason            :: Maybe Text
    --   flag               :: FlagColor
    --   patchCategory      :: Maybe CategoryId  (category to write to YNAB)
    finish kind chosenId mS suggestedIdForName mReason flag patchCategory =
      -- `let ... in ...`: the `let` block defines local variables (each
      -- `name = expression`), and the expression after `in` is the value of the
      -- whole thing. Like `where`, but placed before the expression it serves.
      -- Variables can't be reassigned: each `=` here defines a name once.
      let -- `fmap f m` applies `f` to the value inside a Maybe if there is one:
          --   fmap sConfidence (Just s) = Just (sConfidence s)
          --   fmap sConfidence Nothing  = Nothing
          -- Result type: Maybe Double.
          confidence = fmap sConfidence mS
          -- `>>=` is the "bind" operator. For Maybe it means: if the left side
          -- is `Just x`, call the right side function on `x` (which itself
          -- returns a Maybe); if it's `Nothing`, the whole thing is `Nothing`.
          -- `clNameOf cats` reads the field `clNameOf` from the `cats` record,
          -- which gives a function of type CategoryId -> Maybe Text.
          -- Result type: Maybe Text.
          suggestedName = suggestedIdForName >>= clNameOf cats
          -- Look up the chosen category's name, and fall back to the
          -- transaction's own label if there's no id or the lookup fails.
          -- `case` and `Just n` work as above, binding the name to `n`.
          -- Result type: Text (not Maybe, because both branches give Text).
          chosenName =
            case chosenId >>= clNameOf cats of
              Just n -> n
              Nothing -> categoryLabel tx
          -- `fromMaybe default m` unwraps a Maybe: the value if it's `Just`,
          -- otherwise `default`. Here "" (empty text) is the default.
          reasonText = fromMaybe "" mReason
          -- Build the "[auto ...] reason || " prefix (see `makeTag` below).
          tag = makeTag kind confidence suggestedName reasonText
          -- Put the tag in front of the original memo, replacing any old tag.
          memo = buildMemo tag (fromMaybe "" (txMemo tx))
          -- RECORD CONSTRUCTION: `TypeName { field = value, ... }` builds a
          -- single `TransactionPatch` value with its named fields filled in.
          -- The `{ , , }` is record syntax, not a list.
          patch =
            TransactionPatch
              { tpId = txId tx
              , tpCategoryId = patchCategory
              , tpMemo = memo
              , tpFlagColor = flag
              }
       -- The value after `in` is what `finish` returns: one `Decision`
       -- record, built with the same record syntax as above.
       in Decision
            { decisionKind = kind
            , decisionTx = tx
            , decisionSuggestedName = suggestedName
            , decisionChosenId = chosenId
            , decisionChosenName = chosenName
            , decisionConfidence = confidence
            , decisionReason = mReason
            , decisionRuleMatched = ruleMatched  -- comes from decideOne's parameter
            , decisionPatch = patch
            }

makeTag :: DecisionKind -> Maybe Double -> Maybe Text -> Text -> Text
makeTag kind confidence suggestedName reason =
  "[auto "
    <> formatConf confidence
    <> " "
    <> kindLabel kind suggestedName
    <> "] "
    <> T.take 80 reason
    <> " || "

kindLabel :: DecisionKind -> Maybe Text -> Text
kindLabel KindConfident _ = "confident"
kindLabel KindUnsupported _ = "unsupported"
kindLabel KindNeedsReview (Just name) = "review: " <> name
kindLabel KindNeedsReview Nothing = "review"

formatConf :: Maybe Double -> Text
formatConf Nothing = "n/a"
formatConf (Just c) =
  T.pack (showFFloat (Just 2) (clamp 0 1 c) "")

clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)

stripAutoTag :: Text -> Text
stripAutoTag memo =
  case T.stripPrefix "[auto " memo of
    Nothing -> memo
    Just rest ->
      case T.breakOn "] " rest of
        (_, afterClose)
          | T.length afterClose < 2 -> memo
          | otherwise ->
              let afterBracket = T.drop 2 afterClose
               in case T.breakOn " || " afterBracket of
                    (_, orig)
                      | T.length orig < 4 -> memo
                      | otherwise -> T.drop 4 orig

buildMemo :: Text -> Text -> Text
buildMemo tag original =
  let stripped = stripAutoTag original
      full = tag <> stripped
   in if T.length full <= 500
        then full
        else
          let room = 500 - T.length tag
           in if room <= 0
                then T.take 500 tag
                else tag <> T.take room stripped
