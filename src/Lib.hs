module Lib
    ( readPrompt, readEvalPrint, loopUntil
    ) where

import System.Environment
import System.IO
import Control.Monad

import Parsers
import Evaluator

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

readEvalPrint :: String -> IO()
readEvalPrint = putStrLn . show . eval . readExpr

loopUntil :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
loopUntil pred prompt action = do {
    x <- prompt;
    if pred x
      then return ()
      else (action x >> loopUntil pred prompt action)
}
