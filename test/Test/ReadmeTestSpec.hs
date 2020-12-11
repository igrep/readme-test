{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ReadmeTestSpec (main, spec) where

import qualified Data.Text.Lazy             as T

import           Control.Monad.State.Strict (runState)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Test.ReadmeTest


main :: IO ()
main = hspec spec

{-
NOTE:

<!-- ReadmeTest: CompareAfterPrompt ByExpression -->

```haskell
> aaa
> bbb
expected line1-1
expected line1-2

> ddd
expected line2
```

into:

readmeTestCompareAfterPrompt35_ = do
  let rtcapT_36 = aaa
  let rtcapT_37 = bbb
  let rtcapE_38 = expected line1-1
                  expected line1-2

  let rtcapT_41 = ddd
  let rtcapE_42 = expected line2
  [rtcapT_37 @?= rtcapE_38, rtcapT_41 @?= rtcapE_42]

<!-- ReadmeTest: CompareAfterPrompt ByPrintedString -->

```haskell
> aaa
> bbb
expected line1-1
expected line1-2

> ddd
expected line2
```

into:

readmeTestCompareAfterPrompt35_ = do
  let rtcapT_36 = show aaa
  let rtcapT_37 = show bbb
  let rtcapE_38 = "expected line1-1"
  let rtcapE_39 = "expected line1-2"

  let rtcapT_41 = show ddd
  let rtcapE_42 = "expected line2"
  [rtcapT_37 @?= unlines [rtcapE_38, rtcapE_39], rtcapT_41 @?= unlines [rtcapE_42]]

# TODO: how to handle IO!
-}


spec :: Spec
spec =
  describe "processLine" $ do
    context "when outside any code block" $ do
      let appStateOutsideCodeBlock = appStateOf False

      let gen = (,,) <$> anyInitialDirective <*> anyLineNumber <*> anyInitialDirective
       in prop "Given a directive line, current directive should be updated" . forAll gen
        $ \(currentDirective, ln, givenDirective) -> do
          let state0 = appStateOutsideCodeBlock currentDirective
              state1 = state0 { currentDirective = givenDirective }
              line = toDirectiveLine givenDirective
              action = processLine "input.md" ln line
          runState action state0 `shouldBe` (("", Nothing), state1)

      let gen = (,) <$> anyInitialDirective <*> anyLineNumber
       in prop "Given a line \"```\", the state shouldn't change" . forAll gen
          $ \(currentDirective, ln) -> do
            let state0 = appStateOutsideCodeBlock currentDirective
                line = "```"
                action = processLine "input.md" ln line
            runState action state0 `shouldBe` (("", Nothing), state0)

      context "when the current directive is AppendAsIs or Ignore" $ do
        let gen = (,) <$> appendAsIsOrIgnore <*> anyLineNumber
         in prop "Given a line representing a beggining of a code block, inCodeBlock should be True" . forAll gen
            $ \(currentDirective, ln) -> do
              let state0 = appStateOutsideCodeBlock currentDirective
                  state1 = state0 { inCodeBlock = True }
                  actionHaskell = processLine "input.md" ln "```haskell"
              runState actionHaskell state0 `shouldBe` (("", Nothing), state1)

              let actionHs = processLine "input.md" ln "```hs"
              runState actionHs state0 `shouldBe` (("", Nothing), state1)

      context "when the current directive is ValidateAsExpression" $ do
        prop "Given a line representing a beggining of a code block, produces the identifier of the test, CompareAfterPrompt gets empty, and inCodeBlock should be True" . forAll anyLineNumber
          $ \ln -> do
            let currentDirective = ValidateAsExpression
                state0 = appStateOutsideCodeBlock currentDirective
                state1 = state0 { inCodeBlock = True }
                actionHaskell = processLine "input.md" ln "```haskell"
                producedLine = "readmeTestValidateAsExpression" <> T.pack (show ln) <> "_ ="
            runState actionHaskell state0 `shouldBe` ((producedLine, Nothing), state1)

      context "when the current directive is CompareAfterPrompt" $ do
        let gen = (,) <$> (CompareAfterPrompt <$> listOf anyComparison <*> anyComparisonSpec) <*> anyLineNumber
         in prop "Given a line representing a beggining of a code block, produces the identifier of the test, CompareAfterPrompt gets empty, and inCodeBlock should be True" . forAll gen
            $ \(currentDirective, ln) -> do
              let state0 = appStateOutsideCodeBlock currentDirective
                  CompareAfterPrompt _ comparisonSpec = currentDirective
                  directive1 = CompareAfterPrompt [] comparisonSpec
                  state1 = state0
                    { inCodeBlock = True
                    , currentDirective = directive1
                    , foundCompareAfterPrompts = [ln]
                    }
                  actionHaskell = processLine "input.md" ln "```haskell"
                  producedLine = "readmeTestCompareAfterPrompt" <> T.pack (show ln) <> "_ = do"
              runState actionHaskell state0 `shouldBe` ((producedLine, Nothing), state1)

              let actionHs = processLine "input.md" ln "```hs"
              runState actionHs state0 `shouldBe` ((producedLine, Nothing), state1)

    context "when inside a code block" $ do
      let appStateInsideCodeBlock = appStateOf True

      context "when the current directive is NOT CompareAfterPrompt" $ do
        let gen = (,) <$> exceptCompareAfterPrompt <*> anyLineNumber
         in prop "Given a line \"```\", the state gets out of a code block" . forAll gen
          $ \(currentDirective, ln) -> do
            let state0 = appStateInsideCodeBlock currentDirective
                state1 = state0 { inCodeBlock = False }
                line = "```"
                action = processLine "input.md" ln line
            runState action state0 `shouldBe` (("", Nothing), state1)

      context "when the current directive is CompareAfterPrompt" $ do
        let gen = (,) <$> (CompareAfterPrompt [] <$> anyComparisonSpec)  <*> anyLineNumber
         in prop "Given a line \"```\", produces expectations of the comparisons, the state gets out of a code block" . forAll gen
            $ \(currentDirective, ln) -> do
              let state0 = appStateInsideCodeBlock currentDirective
                  state1 = state0 { inCodeBlock = False }
                  line = "```"
                  action = processLine "input.md" ln line
                  CompareAfterPrompt comparisons _ = currentDirective
                  producedLine = formatExpectations comparisons
              runState action state0 `shouldBe` ((producedLine, Nothing), state1)

      context "when the current directive is Ignore" $ do
        let currentDirective = Ignore

        let gen = (,) <$> anyNonCodeBlockLine <*> anyLineNumber
         in prop "Given a line except \"```\", the state shouldn't change" . forAll gen
          $ \(line, ln) -> do
            let state0 = appStateInsideCodeBlock currentDirective
                action = processLine "input.md" ln line
            runState action state0 `shouldBe` (("", Nothing), state0)

      context "when the current directive is AppendAsIs" $ do
        let currentDirective = AppendAsIs

        let gen = (,) <$> anyNonCodeBlockLine <*> anyLineNumber
         in prop "Given a line except \"```\", the state shouldn't change, and produces the given line as is" . forAll gen
          $ \(line, ln) -> do
            let state0 = appStateInsideCodeBlock currentDirective
                action = processLine "input.md" ln line
            runState action state0 `shouldBe` ((line, Nothing), state0)

      context "when the current directive is ValidateAsExpression" $ do
        let currentDirective = ValidateAsExpression

        let gen = (,) <$> anyNonCodeBlockLine <*> anyLineNumber
         in prop "Given a line except \"```\", the state shouldn't change, and produces the given line with an indentation" . forAll gen
          $ \(line, ln) -> do
            let state0 = appStateInsideCodeBlock currentDirective
                action = processLine "input.md" ln line
                producedLine = "  " <> line
            runState action state0 `shouldBe` ((producedLine, Nothing), state0)

      context "when the current directive is CompareAfterPrompt" $ do
        let aNewComparisonShouldBeAdded :: HasCallStack => Gen Directive -> Spec
            aNewComparisonShouldBeAdded genDirective =
              let gen = (,,) <$> genDirective <*> lineToTestBegin <*> anyLineNumber
               in prop "Given a line prefixed with '>', a new Comparison should be added to the current CompareAfterPrompt." . forAll gen
                $ \(currentDirective, line, ln) -> do
                  let state0 = appStateInsideCodeBlock currentDirective
                      action = processLine "input.md" ln line
                      withoutPrompt = T.drop (T.length prefixToTestBegin) line
                      producedLine = "  let rtcapT_" <> T.pack (show ln) <> " = " <> withoutPrompt
                      CompareAfterPrompt comparisons comparisonSpec = currentDirective
                      newComparison = Comparison ln Nothing
                      state1 = state0 { currentDirective = CompareAfterPrompt (newComparison : comparisons) comparisonSpec }
                  runState action state0 `shouldBe` ((producedLine, Nothing), state1)

        let theLineShouldBeTreatedAsAComment :: HasCallStack => Gen Directive -> Spec
            theLineShouldBeTreatedAsAComment genDirective =
              let gen = (,,) <$> genDirective <*> commentLine <*> anyLineNumber
               in prop "Given a line prefixed with '-- ', the line should be treated as a comment." . forAll gen
                $ \(currentDirective, line, ln) -> do
                  let state0 = appStateInsideCodeBlock currentDirective
                      action = processLine "input.md" ln line
                  runState action state0 `shouldBe` ((line, Nothing), state0)

        context "when no comparison is yet found" $ do
          let genDirective = CompareAfterPrompt [] <$> anyComparisonSpec

          aNewComparisonShouldBeAdded genDirective
          theLineShouldBeTreatedAsAComment genDirective

          let gen = (,,) <$> genDirective <*> anyOtherLine <*> anyLineNumber
           in prop "Given a line not prefixed with either '>' nor '-- ', a warning should be produced." . forAll gen
            $ \(currentDirective, line, ln) -> do
              let state0 = appStateInsideCodeBlock currentDirective
                  action = processLine "input.md" ln line
                  warn =
                    "Invalid line in a CompareAfterPrompt block at "
                    ++ show ln
                    ++ ": "
                    ++ show line
                    ++ "\n  Found an expected line before lines to test!"
              runState action state0 `shouldBe` (("", Just warn), state0)

        context "when the line to test is already found" $ do
          context "when the first expected line is already found" $ do
            let genDirective = CompareAfterPrompt <$> (listOf1 anyComparisonFilled) <*> anyComparisonSpec

            aNewComparisonShouldBeAdded genDirective
            theLineShouldBeTreatedAsAComment genDirective

            let gen = (,,) <$> genDirective <*> anyOtherLine <*> anyLineNumber
             in prop "Given a line not prefixed with either '>' nor '-- ', the produced line should be a continuing expected line" . forAll gen
              $ \(currentDirective, line, ln) -> do
                let state0 = appStateInsideCodeBlock currentDirective
                    action = processLine "input.md" ln line
                    CompareAfterPrompt (Comparison { firstExpectedLineAt = Just expectedLineAt } : _) _ =
                      currentDirective
                    letRtcapE = "let rtcapE_" <> T.pack (show expectedLineAt)  <> " = "
                    producedLine = "  " <> T.replicate (T.length letRtcapE) " " <> line
                runState action state0 `shouldBe` ((producedLine, Nothing), state0)

          context "when the first expected line isn't yet found" $ do
            let genDirective = CompareAfterPrompt <$> listOf1 anyComparisonNoExpectedLine <*> anyComparisonSpec

            aNewComparisonShouldBeAdded genDirective

            let gen = (,,) <$> genDirective <*> anyOtherLine <*> anyLineNumber
             in prop "Given a line not prefixed with either '>' nor '-- ', the produced line should be the first expected line" . forAll gen
              $ \(currentDirective, line, ln) -> do
                let state0 = appStateInsideCodeBlock currentDirective
                    action = processLine "input.md" ln line
                    producedLine = "  let rtcapE_" <> T.pack (show ln) <> " = " <> line

                    CompareAfterPrompt (currentComparison : comparisons) comparisonSpec = currentDirective
                    newComparison = currentComparison { firstExpectedLineAt = Just ln }
                    newCompareAfterPrompt = CompareAfterPrompt (newComparison : comparisons) comparisonSpec
                    state1 = state0 { currentDirective = newCompareAfterPrompt }
                runState action state0 `shouldBe` ((producedLine, Nothing), state1)

            theLineShouldBeTreatedAsAComment genDirective



appStateOf :: Bool -> Directive -> AppState
appStateOf inCodeBlock currentDirective =
  AppState { inCodeBlock, currentDirective, foundCompareAfterPrompts = [] }


appendAsIsOrIgnore :: Gen Directive
appendAsIsOrIgnore = elements [Ignore, AppendAsIs]


exceptCompareAfterPrompt :: Gen Directive
exceptCompareAfterPrompt = elements [Ignore, AppendAsIs, ValidateAsExpression]


anyInitialDirective :: Gen Directive
anyInitialDirective = oneof [exceptCompareAfterPrompt, anyCompareAfterPrompt]
 where
  anyCompareAfterPrompt = CompareAfterPrompt [] <$> anyComparisonSpec


anyComparisonNoExpectedLine :: Gen Comparison
anyComparisonNoExpectedLine =
  Comparison <$> anyLineNumber <*> pure Nothing


anyComparisonFilled :: Gen Comparison
anyComparisonFilled =
  Comparison <$> anyLineNumber <*> (Just <$> anyLineNumber)


anyComparison :: Gen Comparison
anyComparison =
  Comparison <$> anyLineNumber <*> liftArbitrary anyLineNumber


anyComparisonSpec :: Gen ComparisonSpec
anyComparisonSpec = pure ByExpression
-- NOTE: ByPrintedString isn't currently supported!
-- anyComparisonSpec = elements [ByPrintedString, ByExpression]


-- The initial lines are a corner case: the prelude line is printed.
anyLineNumber :: Gen LineNumber
anyLineNumber = choose (3, maxBound)


toDirectiveLine :: Directive -> T.Text
toDirectiveLine d =
  case d of
      Ignore -> directiveLineOf $ show d
      AppendAsIs -> directiveLineOf $ show d
      ValidateAsExpression -> directiveLineOf $ show d
      CompareAfterPrompt _ csp ->
        directiveLineOf $ "CompareAfterPrompt " ++ show csp
 where
  directiveLineOf contents =
    "<!-- ReadmeTest: " <> T.pack contents <> " -->"


commentLine :: Gen T.Text
commentLine = ("-- " <>) <$> anyNonCodeBlockLine


lineToTestBegin :: Gen T.Text
lineToTestBegin = (prefixToTestBegin <>) <$> anyNonCodeBlockLine


prefixToTestBegin :: T.Text
prefixToTestBegin = ">"


anyNonCodeBlockLine :: Gen T.Text
anyNonCodeBlockLine = fmap T.pack . listOf . elements $ filter (/= '`') ['!' .. ',']


anyOtherLine :: Gen T.Text
anyOtherLine =
  fmap T.pack . listOf1 . elements $ filter (`notElem` (" -<`" :: String)) ['!' .. ',']
