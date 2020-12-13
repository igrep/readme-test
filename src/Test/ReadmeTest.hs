{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

-- | Convert markdown files into HUnit-based test files using 'Directive's.
module Test.ReadmeTest
  ( AppState (..)
  , Directive (..)
  , Comparison (..)
  , ComparisonSpec (..)
  , LineNumber
  , convertThenRun
  , convertFile
  , processLine
  , parseLine
  , parseLineInComparison
  , formatExpectations
  , generateMain
  )
where

import           Control.Applicative        ((<|>))
import           Control.Error.Util         (note)
import           Control.Monad              (guard, zipWithM, (<=<))
-- import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (MonadState, get, modify', put,
                                             runStateT)
import           Data.Bifunctor             (first)
import           Data.Foldable              (for_)
import           Data.Maybe                 (fromMaybe, mapMaybe)
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.IO          as TI
import           GHC.Generics               (Generic)
import           System.FilePath            (replaceExtension)
import           System.IO                  (IOMode (WriteMode), withFile)
import           System.Process.Typed       (proc, runProcess_)


data AppState = AppState
  { currentDirective         :: Directive
  , foundCompareAfterPrompts :: [LineNumber]
  -- ^ Used for the labels of the generated CompareAfterPrompt blocks
  , inCodeBlock              :: Bool
  }
  deriving (Eq, Show, Generic)


data Directive =
    Ignore
  | AppendAsIs
  | ValidateAsExpression
  | CompareAfterPrompt [Comparison] ComparisonSpec
  deriving (Eq, Show, Generic)


data Comparison = Comparison
  { beginsAt            :: LineNumber
  -- ^ Used for the label of the generated test
  , firstExpectedLineAt :: Maybe Int
  -- ^ If Nothing, the line to test isn't actually compared: treated like 'ValidateAsExpression'
  }
  deriving (Eq, Show, Generic)


data ComparisonSpec = ByPrintedString | ByExpression
  deriving (Eq, Show, Generic)


type LineNumber = Int


data ParsedLine =
  Unrelated | FoundDirective Directive | BeginBlock | EndBlock
  deriving (Eq, Show, Generic)


data ParsedLineInComparison =
  Comment | LineToTest T.Text | ExpectedLine
  deriving (Eq, Show, Generic)


type Warning = String


defaultDirective :: Directive
defaultDirective = AppendAsIs


-- NOTE: Perhaps I should have written this function with a parser combinator...
parseLine :: T.Text -> Either Warning ParsedLine
parseLine = f . T.stripEnd
 where
  f "```haskell" = Right BeginBlock
  f "```hs" = Right BeginBlock
  f "```" = Right EndBlock
  f line =
    case dropDirectiveBegin line of
        Just left ->
          FoundDirective <$> parseDirectiveWithoutBegin left
        Nothing -> Right Unrelated


dropDirectiveBegin :: T.Text -> Maybe T.Text
dropDirectiveBegin =
  T.stripPrefix "ReadmeTest:" . T.stripStart
    <=< T.stripPrefix "<!--" . T.stripStart


parseDirectiveWithoutBegin :: T.Text -> Either Warning Directive
parseDirectiveWithoutBegin = assertNoExtraCharactersLeft <=< f . T.stripStart
 where
  f t =
    ((,) Ignore <$> ignoreD t)
      <|> ((,) AppendAsIs <$> appendAsIsD t)
      <|> ((,) ValidateAsExpression <$> onlyValidateD t)
      <|> compareAfterPromptD t

  ignoreD = note "Invalid Ignore directive" . T.stripPrefix "Ignore"
  appendAsIsD = note "Invalid AppendAsIs directive" . T.stripPrefix "AppendAsIs"
  onlyValidateD = note "Invalid ValidateAsExpression directive" . T.stripPrefix "ValidateAsExpression"
  compareAfterPromptD t = do
    let emsg1 = "Unknown directive: " ++ show t ++ "."
    argsLeft <- note emsg1 . fmap T.stripStart $ T.stripPrefix "CompareAfterPrompt" t

    let emsg2 = "Unknown option of CompareAfterPrompt: " ++ show argsLeft ++ "."
        byPrintedStringCS = (,) ByPrintedString <$> T.stripPrefix "ByPrintedString" argsLeft
        byExpressionCS = (,) ByExpression <$> T.stripPrefix "ByExpression" argsLeft
    note emsg2 . fmap (first (CompareAfterPrompt [])) $ byPrintedStringCS <|> byExpressionCS

  assertNoExtraCharactersLeft (directive, afterDirective) = do
    let t = T.stripStart afterDirective
    if "-->" `T.isPrefixOf` t || T.null t
      then return directive
      else Left $ "Unexpected characters left: " ++ show afterDirective



parseLineInComparison :: T.Text -> ParsedLineInComparison
parseLineInComparison = fromMaybe ExpectedLine . f
 where
  f line =
    (Comment <$ guard ("--" `T.isPrefixOf` line))
      <|> (LineToTest <$> T.stripPrefix ">" line)


-- Command name to run the converted file
-- Currently this program just converts the markdown file: doesn't run the generated test file.
-- type Runner = String


initialState :: AppState
initialState = AppState { currentDirective = defaultDirective, foundCompareAfterPrompts = [], inCodeBlock = False }


convertThenRun :: FilePath -> IO ()
convertThenRun srcPath = do
  destPath <- convertFile srcPath
  runProcess_ $ proc "stack" ["exec", "runghc", "--", destPath]


convertFile :: FilePath -> IO FilePath
convertFile srcPath = do
  let destPath = replaceExtension srcPath "hs"
  (results, appState) <-
    (`runStateT` initialState)
      . zipWithM (processLine srcPath) [1..]
      . T.lines
      =<< TI.readFile srcPath
  withFile destPath WriteMode $ \hd -> do
    for_ results $ \(line, merr) -> do
      for_ merr $ \err -> putStrLn $ "WARN: " ++ err
      TI.hPutStrLn hd line
    TI.hPutStrLn hd $ generateMain appState
  return destPath


processLine :: MonadState AppState m => FilePath -> LineNumber -> T.Text -> m (T.Text, Maybe Warning)
processLine _path ln line = do
  current@AppState { currentDirective, inCodeBlock } <- get
  if inCodeBlock
    then
      case parseLine line of
          Right EndBlock -> do
            put $ current { inCodeBlock = False }
            case currentDirective of
                Ignore                 -> returnEmpty
                AppendAsIs             -> returnEmpty
                ValidateAsExpression   -> returnEmpty
                CompareAfterPrompt comparisons spec -> do
                  case spec of
                      ByPrintedString -> error "Sorry, ByPrintedString is not supported yet!"
                      ByExpression ->
                        returnLine $ formatExpectations comparisons
          Right Unrelated ->
            case currentDirective of
                Ignore                 -> returnEmpty
                AppendAsIs             -> returnLine line
                ValidateAsExpression   -> returnLine $ indent <> line
                CompareAfterPrompt comparisons spec ->
                  case parseLineInComparison line of
                      LineToTest lineParsed -> do
                        let newComparison = Comparison { beginsAt = ln, firstExpectedLineAt = Nothing }
                            newDirective = CompareAfterPrompt (newComparison : comparisons) spec
                        modify' (\appState -> appState { currentDirective = newDirective })
                        returnLine $ indent <> "let rtcapT_" <> lnT <> " = " <> lineParsed

                      ExpectedLine         ->
                        case comparisons of
                            [] ->
                              returnWarning
                                $ "Invalid line in a CompareAfterPrompt block at "
                                ++ show ln
                                ++ ": "
                                ++ show line
                                ++ "\n  Found an expected line before lines to test!"
                            (currentComparison : otherComparisons) -> do
                              case firstExpectedLineAt currentComparison of
                                  Nothing -> do
                                    let newComparison = currentComparison { firstExpectedLineAt = Just ln }
                                        newDirective = CompareAfterPrompt (newComparison : otherComparisons) spec
                                    modify' (\appState -> appState { currentDirective = newDirective })
                                    let letRtcapE = "let rtcapE_" <> lnT <> " = "
                                    returnLine $ indent <> letRtcapE <> line
                                  Just expectedLineAt -> do
                                    let letRtcapE = "let rtcapE_" <> T.pack (show expectedLineAt)  <> " = "
                                    returnLine $ indent <> T.replicate (T.length letRtcapE) " " <> line
                      Comment              -> returnLine line
          Right _other -> returnEmpty -- NOTE: Maybe I should make `parseLineInCodeBlock`
          Left warning -> returnWarning warning
    else
      case parseLine line of
          Right Unrelated -> returnEmpty
          Right BeginBlock -> do
            put $ current { inCodeBlock = True }
            case currentDirective of
                Ignore                 -> returnEmpty
                AppendAsIs             -> returnEmpty
                ValidateAsExpression   -> returnLine $ "readmeTestValidateAsExpression" <> lnT <> "_ ="
                CompareAfterPrompt _ spec -> do
                  modify' $ \state' -> state'
                    { currentDirective = CompareAfterPrompt [] spec
                    , foundCompareAfterPrompts = ln : foundCompareAfterPrompts state'
                    }
                  returnLine $ "readmeTestCompareAfterPrompt" <> lnT <> "_ = do"
          Right (FoundDirective dir) -> do
            put $ current { currentDirective = dir }
            returnEmpty
          Right EndBlock -> returnEmpty
          Left warning -> returnWarning warning
 where
  returnEmpty = return (preludeLine, Nothing)
  returnLine t =
    if withinPreludeArea
      then return (preludeLine <> "\n" <> t, Just "Producing prelude line with the converted lines")
      else return (t, Nothing)
  returnWarning w = return (preludeLine, Just w)
  lnT = T.pack (show ln)

  withinPreludeArea = ln <= length preludeLines
  preludeLine = if withinPreludeArea then preludeLines !! (ln - 1) else ""
  preludeLines =
    [ "import qualified Test.HUnit as HUnit"
    , "import System.Exit (exitSuccess, exitFailure)"
    ]


-- Public only for testing
formatExpectations :: [Comparison] -> T.Text
formatExpectations comparisons =
  indent <> "HUnit.TestList [" <> T.intercalate ", " (mapMaybe f comparisons) <> "]"
 where
  f Comparison { firstExpectedLineAt = Nothing } = Nothing
  f Comparison { beginsAt, firstExpectedLineAt = Just expectedLineAt } = do
    let beginsAtT = T.pack (show beginsAt)
        idTest = "rtcapT_" <> beginsAtT
        idExpected = "rtcapE_" <> T.pack (show expectedLineAt)
        description = "\"comparison at " <> beginsAtT <> "\""
    Just $ "HUnit.TestCase (HUnit.assertEqual " <> description <> " " <> idExpected <> "  " <> idTest <> ")"

indent :: T.Text
indent = "  "


generateMain :: AppState -> T.Text
generateMain AppState { foundCompareAfterPrompts } = mainLines <> testsLines <> runTestTTAndExitLines
 where
  mainLines = T.unlines
    [ ""
    , "main :: IO ()"
    , "main = runTestTTAndExit $ HUnit.TestList"
    ]
  testsLines = braOpen <> T.intercalate sep (map f foundCompareAfterPrompts) <> braClose
  f comparisonLn = "readmeTestCompareAfterPrompt" <> T.pack (show comparisonLn) <> "_"
  braOpen = indent <> "[ "
  braClose = "\n" <> indent <> "]"
  sep = "\n" <> indent <> ", "

  runTestTTAndExitLines = T.unlines
    [ "\n where"
    , "  runTestTTAndExit :: HUnit.Test -> IO ()"
    , "  runTestTTAndExit tests = do"
    , "    c <- HUnit.runTestTT tests"
    , "    if (HUnit.errors c == 0) && (HUnit.failures c == 0)"
    , "      then exitSuccess"
    , "      else exitFailure"
    ]
