module Main where

import System.Environment (getArgs)

import Test.ReadmeTest

main :: IO ()
main = readmeTestWith "drivers/ruby" =<< getArgs
