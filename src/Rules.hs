module Rules
  ( RawRules (..)
  , RawRule (..)
  , Rules (..)
  , ResolvedRule (..)
  , CandidateCategory (..)
  , parseRawRules
  , loadRulesFile
  , resolveRules
  , candidateCategories
  , matchRule
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (ParseException, decodeEither')
import System.IO.Error (isDoesNotExistError)
import Text.Regex.TDFA (Regex, defaultCompOpt, defaultExecOpt, makeRegexOptsM, matchTest)
import Text.Regex.TDFA.Common (CompOption (..))
import Ynab.Types
  ( CategoryId
  , YnabCategory (..)
  , YnabCategoryGroup (..)
  )

data RawRules = RawRules
  { rawHints :: [Text]
  , rawRules :: [RawRule]
  , rawExcludeCategories :: [Text]
  }
  deriving (Eq, Show)

data RawRule = RawRule
  { rawPayeeContains :: Maybe Text
  , rawPayeeRegex :: Maybe Text
  , rawMinAmount :: Maybe Scientific
  , rawMaxAmount :: Maybe Scientific
  , rawCategory :: Text
  , rawNote :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON RawRule where
  parseJSON = withObject "rule" $ \o ->
    RawRule
      <$> o .:? "payeeContains"
      <*> o .:? "payeeRegex"
      <*> o .:? "minAmount"
      <*> o .:? "maxAmount"
      <*> o .: "category"
      <*> o .:? "note"

instance FromJSON RawRules where
  parseJSON = withObject "rules" $ \o ->
    RawRules
      <$> o .:? "hints" .!= []
      <*> o .:? "rules" .!= []
      <*> o .:? "excludeCategories" .!= []

data ExcludePattern
  = ExcludeGroup Text
  | ExcludeExact Text Text
  | ExcludeName Text
  deriving (Eq, Show)

data PayeeMatch
  = MatchContains Text
  | MatchRegex Regex Text

data ResolvedRule = ResolvedRule
  { rrMatch :: PayeeMatch
  , rrMinMilli :: Maybe Integer
  , rrMaxMilli :: Maybe Integer
  , rrCategoryId :: CategoryId
  , rrCategoryName :: Text
  , rrNote :: Maybe Text
  }

data Rules = Rules
  { rulesHints :: [Text]
  , rulesMatchers :: [ResolvedRule]
  , rulesExcludes :: [ExcludePattern]
  }

data CandidateCategory = CandidateCategory
  { ccId :: CategoryId
  , ccGroup :: Text
  , ccName :: Text
  }
  deriving (Eq, Show)

instance ToJSON CandidateCategory where
  toJSON c =
    object
      [ "id" .= ccId c
      , "group" .= ccGroup c
      , "name" .= ccName c
      ]

parseRawRules :: ByteString -> Either String RawRules
parseRawRules bytes =
  first show (decodeEither' bytes :: Either ParseException RawRules)

loadRulesFile :: FilePath -> [YnabCategoryGroup] -> IO (Either String Rules)
loadRulesFile path groups = do
  result <- try (BS.readFile path) :: IO (Either IOException ByteString)
  case result of
    Left err
      | isDoesNotExistError err ->
          pure $ Left $ "Rules file not found: " <> path
      | otherwise -> pure $ Left (show err)
    Right bytes ->
      pure $ do
        raw <- parseRawRules bytes
        resolveRules groups raw

resolveRules :: [YnabCategoryGroup] -> RawRules -> Either String Rules
resolveRules groups raw = do
  excludes <- traverse parseExclude (rawExcludeCategories raw)
  let candidates = filterCandidates excludes groups
  resolved <- traverse (resolveOne candidates) (rawRules raw)
  pure
    Rules
      { rulesHints = rawHints raw
      , rulesMatchers = resolved
      , rulesExcludes = excludes
      }

candidateCategories :: Rules -> [YnabCategoryGroup] -> [CandidateCategory]
candidateCategories rules = filterCandidates (rulesExcludes rules)

matchRule :: Rules -> Text -> Integer -> Maybe ResolvedRule
matchRule rules payee amountMilli = go (rulesMatchers rules)
  where
    price = abs amountMilli
    go [] = Nothing
    go (rule : rest)
      | matchesPayee (rrMatch rule) payee && withinAmount rule price = Just rule
      | otherwise = go rest

withinAmount :: ResolvedRule -> Integer -> Bool
withinAmount rule price =
  maybe True (price >=) (rrMinMilli rule)
    && maybe True (price <=) (rrMaxMilli rule)

matchesPayee :: PayeeMatch -> Text -> Bool
matchesPayee (MatchContains needle) payee =
  T.toCaseFold needle `T.isInfixOf` T.toCaseFold payee
matchesPayee (MatchRegex regex _) payee =
  matchTest regex (T.unpack payee)

resolveOne :: [CandidateCategory] -> RawRule -> Either String ResolvedRule
resolveOne candidates rule = do
  matcher <- compileMatcher rule
  (minMilli, maxMilli) <- amountWindow rule
  (cid, name) <- resolveCategoryName candidates (rawCategory rule)
  pure
    ResolvedRule
      { rrMatch = matcher
      , rrMinMilli = minMilli
      , rrMaxMilli = maxMilli
      , rrCategoryId = cid
      , rrCategoryName = name
      , rrNote = rawNote rule
      }

amountWindow :: RawRule -> Either String (Maybe Integer, Maybe Integer)
amountWindow rule = do
  minMilli <- traverse dollarsToMilli (rawMinAmount rule)
  maxMilli <- traverse dollarsToMilli (rawMaxAmount rule)
  case (minMilli, maxMilli) of
    (Just lo, Just hi) | lo > hi -> Left "rule minAmount cannot exceed maxAmount"
    _ -> Right (minMilli, maxMilli)

dollarsToMilli :: Scientific -> Either String Integer
dollarsToMilli dollars
  | dollars < 0 = Left "rule amount must not be negative"
  | otherwise =
      case floatingOrInteger (dollars * 1000) :: Either Double Integer of
        Right milli -> Right milli
        Left _ -> Left "rule amount must be a multiple of 0.001 dollars"

compileMatcher :: RawRule -> Either String PayeeMatch
compileMatcher rule =
  case (rawPayeeContains rule, rawPayeeRegex rule) of
    (Just needle, Nothing) ->
      if T.null (T.strip needle)
        then Left "payeeContains must not be empty"
        else Right (MatchContains needle)
    (Nothing, Just pat) ->
      MatchRegex <$> compileRegex pat <*> pure pat
    (Just _, Just _) ->
      Left "a rule cannot have both payeeContains and payeeRegex"
    (Nothing, Nothing) ->
      Left "a rule needs payeeContains or payeeRegex"

compileRegex :: Text -> Either String Regex
compileRegex pat =
  let comp = defaultCompOpt {caseSensitive = False, multiline = False}
      compiled = makeRegexOptsM comp defaultExecOpt (T.unpack pat) :: Maybe Regex
   in case compiled of
        Nothing -> Left $ "invalid payeeRegex: " <> T.unpack pat
        Just regex -> Right regex

resolveCategoryName :: [CandidateCategory] -> Text -> Either String (CategoryId, Text)
resolveCategoryName candidates ref
  | T.null (T.strip ref) = Left "rule category must not be empty"
  | otherwise =
      case splitGroupName ref of
        Just (group, name) ->
          unique ref [c | c <- candidates, ciEq (ccGroup c) group, ciEq (ccName c) name]
        Nothing ->
          unique ref [c | c <- candidates, ciEq (ccName c) ref]

splitGroupName :: Text -> Maybe (Text, Text)
splitGroupName ref =
  case T.breakOn "/" ref of
    (group, rest)
      | T.length rest > 1 && T.head rest == '/' ->
          Just (group, T.drop 1 rest)
    _ -> Nothing

unique :: Text -> [CandidateCategory] -> Either String (CategoryId, Text)
unique ref hits =
  case hits of
    [c] -> Right (ccId c, ccName c)
    [] -> Left $ "Unknown category: " <> T.unpack ref
    _ ->
      Left $
        "Category name "
          <> T.unpack ref
          <> " is ambiguous; use Group/Name"

parseExclude :: Text -> Either String ExcludePattern
parseExclude raw =
  case splitGroupName raw of
    Just (group, "*") -> Right (ExcludeGroup group)
    Just (group, name) -> Right (ExcludeExact group name)
    Nothing -> Right (ExcludeName raw)

filterCandidates :: [ExcludePattern] -> [YnabCategoryGroup] -> [CandidateCategory]
filterCandidates excludes groups =
  [ CandidateCategory
      { ccId = catId cat
      , ccGroup = cgName group
      , ccName = catName cat
      }
  | group <- groups
  , cat <- cgCategories group
  , isCandidate excludes group cat
  ]

isCandidate :: [ExcludePattern] -> YnabCategoryGroup -> YnabCategory -> Bool
isCandidate excludes group cat
  | catDeleted cat = False
  | catHidden cat = False
  | cgDeleted group = False
  | cgHidden group = False
  | ciEq (cgName group) "Credit Card Payments" = False
  | catInternal cat && not (ciEq (catName cat) "Inflow: Ready to Assign") = False
  | any (matchesExclude group cat) excludes = False
  | otherwise = True

matchesExclude :: YnabCategoryGroup -> YnabCategory -> ExcludePattern -> Bool
matchesExclude group cat pat =
  case pat of
    ExcludeGroup g -> ciEq (cgName group) g
    ExcludeExact g n -> ciEq (cgName group) g && ciEq (catName cat) n
    ExcludeName n -> ciEq (catName cat) n

ciEq :: Text -> Text -> Bool
ciEq a b = T.toCaseFold a == T.toCaseFold b
