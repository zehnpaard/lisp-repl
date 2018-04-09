module Main where

import System.IO

import Lib

main :: IO ()
main = loopUntil (== "quit") (readPrompt ">> ") readEvalPrint
