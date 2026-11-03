module RulesSpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Rules
import Test.Hspec
import Ynab.Types

spec :: Spec
spec = do
  describe "parseRawRules" $ do
    it "parses the YAML shape from the plan" $ do
      let yaml =
            BS8.unlines
              [ "hints:"
              , "  - Venmo is usually dining"
              , "rules:"
              , "  - payeeContains: COSTCO"
              , "    category: Groceries"
              , "    note: bulk food"
              , "  - payeeRegex: ^SHELL|^CHEVRON"
              , "    category: Gas"
              , "excludeCategories:"
              , "  - Credit Card Payments/*"
              ]
      case parseRawRules yaml of
        Left err -> expectationFailure err
        Right raw -> do
          rawHints raw `shouldBe` ["Venmo is usually dining"]
          length (rawRules raw) `shouldBe` 2
          rawExcludeCategories raw `shouldBe` ["Credit Card Payments/*"]

  describe "resolveRules" $ do
    it "resolves category names to ids and matches payees" $ do
      case resolveRules sampleGroups sampleRaw of
        Left err -> expectationFailure err
        Right rules -> do
          ccName <$> candidateCategories rules sampleGroups
            `shouldMatchList` ["Groceries", "Gas", "Dining", "Inflow: Ready to Assign"]
          fmap rrCategoryName (matchRule rules "Costco #123" (-50000))
            `shouldBe` Just "Groceries"
          fmap rrCategoryName (matchRule rules "SHELL OIL" (-50000))
            `shouldBe` Just "Gas"
          case matchRule rules "RANDOM STORE" (-50000) of
            Nothing -> pure ()
            Just _ -> expectationFailure "expected no match"

    it "fails on an unknown category name" $ do
      let raw = sampleRaw {rawRules = [containsRule "COSTCO" "Not A Category"]}
      case resolveRules sampleGroups raw of
        Left err -> err `shouldBe` "Unknown category: Not A Category"
        Right _ -> expectationFailure "expected unknown category to fail"

    it "fails on an ambiguous category name" $ do
      let groups = sampleGroups ++ [group "Other" [cat "dup-2" "g-other" "Groceries" False False]]
          raw = sampleRaw {rawRules = [containsRule "COSTCO" "Groceries"]}
      case resolveRules groups raw of
        Left err -> err `shouldContain` "ambiguous"
        Right _ -> expectationFailure "expected ambiguous category to fail"

    it "resolves Group/Name when names collide" $ do
      let groups = sampleGroups ++ [group "Other" [cat "dup-2" "g-other" "Groceries" False False]]
          raw = sampleRaw {rawRules = [containsRule "COSTCO" "Food/Groceries"]}
      case resolveRules groups raw of
        Left err -> expectationFailure err
        Right rules ->
          fmap rrCategoryId (matchRule rules "COSTCO" (-50000)) `shouldBe` Just "cat-groc"

    it "matches a payee by inclusive dollar amount, using the absolute price" $ do
      let yaml =
            BS8.unlines
              [ "rules:"
              , "  - payeeContains: KROGER"
              , "    minAmount: 40"
              , "    maxAmount: 60"
              , "    category: Gas"
              , "  - payeeContains: KROGER"
              , "    minAmount: 150"
              , "    category: Groceries"
              , "  - payeeContains: KROGER"
              , "    maxAmount: 39.999"
              , "    category: Family Spending"
              , "  - payeeContains: KROGER"
              , "    minAmount: 60.001"
              , "    maxAmount: 149.999"
              , "    category: Family Spending"
              ]
          groups =
            sampleGroups
              ++ [group "Wants" [cat "cat-fam" "g-wants" "Family Spending" False False]]
      case parseRawRules yaml >>= resolveRules groups of
        Left err -> expectationFailure err
        Right rules -> do
          fmap rrCategoryName (matchRule rules "KROGER FUEL" (-40000))
            `shouldBe` Just "Gas"
          fmap rrCategoryName (matchRule rules "kroger" (-60000))
            `shouldBe` Just "Gas"
          fmap rrCategoryName (matchRule rules "KROGER" 50000)
            `shouldBe` Just "Gas"
          fmap rrCategoryName (matchRule rules "KROGER" (-150000))
            `shouldBe` Just "Groceries"
          fmap rrCategoryName (matchRule rules "KROGER" (-39999))
            `shouldBe` Just "Family Spending"
          fmap rrCategoryName (matchRule rules "KROGER" (-100000))
            `shouldBe` Just "Family Spending"
          fmap rrCategoryName (matchRule rules "KROGER" (-60001))
            `shouldBe` Just "Family Spending"
          fmap rrCategoryName (matchRule rules "KROGER" (-149999))
            `shouldBe` Just "Family Spending"

    it "rejects an inverted, negative, or sub-milliunit amount" $ do
      let inverted =
            BS8.unlines
              [ "rules:"
              , "  - payeeContains: KROGER"
              , "    minAmount: 80"
              , "    maxAmount: 10"
              , "    category: Gas"
              ]
          negative =
            BS8.unlines
              [ "rules:"
              , "  - payeeContains: KROGER"
              , "    minAmount: -1"
              , "    category: Gas"
              ]
          fraction =
            BS8.unlines
              [ "rules:"
              , "  - payeeContains: KROGER"
              , "    minAmount: 10.0001"
              , "    category: Gas"
              ]
      case parseRawRules inverted >>= resolveRules sampleGroups of
        Left err -> err `shouldBe` "rule minAmount cannot exceed maxAmount"
        Right _ -> expectationFailure "expected inverted amounts to fail"
      case parseRawRules negative >>= resolveRules sampleGroups of
        Left err -> err `shouldBe` "rule amount must not be negative"
        Right _ -> expectationFailure "expected a negative amount to fail"
      case parseRawRules fraction >>= resolveRules sampleGroups of
        Left err -> err `shouldBe` "rule amount must be a multiple of 0.001 dollars"
        Right _ -> expectationFailure "expected a sub-milliunit amount to fail"

    it "excludes hidden, deleted, credit-card, and other internal categories" $ do
      case resolveRules sampleGroups emptyRaw of
        Left err -> expectationFailure err
        Right rules ->
          map ccName (candidateCategories rules sampleGroups)
            `shouldNotContain` ["Hidden Cat", "Deleted Cat", "CC Payment", "Uncategorized"]

sampleRaw :: RawRules
sampleRaw =
  RawRules
    { rawHints = ["Venmo is usually dining"]
    , rawRules =
        [ containsRule "COSTCO" "Groceries"
        , regexRule "^SHELL|^CHEVRON" "Gas"
        ]
    , rawExcludeCategories = ["Credit Card Payments/*"]
    }

emptyRaw :: RawRules
emptyRaw = RawRules [] [] ["Credit Card Payments/*"]

containsRule :: Text -> Text -> RawRule
containsRule needle categoryName =
  RawRule
    { rawPayeeContains = Just needle
    , rawPayeeRegex = Nothing
    , rawMinAmount = Nothing
    , rawMaxAmount = Nothing
    , rawCategory = categoryName
    , rawNote = Just "bulk food"
    }

regexRule :: Text -> Text -> RawRule
regexRule pat categoryName =
  RawRule
    { rawPayeeContains = Nothing
    , rawPayeeRegex = Just pat
    , rawMinAmount = Nothing
    , rawMaxAmount = Nothing
    , rawCategory = categoryName
    , rawNote = Nothing
    }

sampleGroups :: [YnabCategoryGroup]
sampleGroups =
  [ group
      "Food"
      [ cat "cat-groc" "g-food" "Groceries" False False
      , cat "cat-din" "g-food" "Dining" False False
      ]
  , group "Auto" [cat "cat-gas" "g-auto" "Gas" False False]
  , group "Internal" [inflow, uncategorized]
  , group "Credit Card Payments" [cat "cat-cc" "g-cc" "CC Payment" False False]
  , hiddenGroup
  , deletedGroup
  ]

inflow :: YnabCategory
inflow = cat "cat-in" "g-int" "Inflow: Ready to Assign" False True

uncategorized :: YnabCategory
uncategorized = cat "cat-uncat" "g-int" "Uncategorized" False True

hiddenGroup :: YnabCategoryGroup
hiddenGroup =
  (group "Hidden" [cat "cat-hid" "g-hid" "Hidden Cat" True False])
    { cgHidden = True
    }

deletedGroup :: YnabCategoryGroup
deletedGroup =
  (group "Old" [cat "cat-del" "g-old" "Deleted Cat" False False])
    { cgDeleted = True
    }

group :: Text -> [YnabCategory] -> YnabCategoryGroup
group name cats =
  YnabCategoryGroup
    { cgId = "g-" <> name
    , cgName = name
    , cgHidden = False
    , cgInternal = False
    , cgDeleted = False
    , cgCategories = cats
    }

cat :: CategoryId -> Text -> Text -> Bool -> Bool -> YnabCategory
cat cid gid name hidden internal =
  YnabCategory
    { catId = cid
    , catGroupId = gid
    , catName = name
    , catHidden = hidden
    , catInternal = internal
    , catDeleted = False
    }
