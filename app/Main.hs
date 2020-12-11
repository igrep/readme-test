module Main where

import           System.Environment (getArgs)
import           Test.ReadmeTest


main :: IO ()
main = getArgs >>= mapM_ convertThenRun
