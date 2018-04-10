module Lib
    ( readEvalPrintLoop
    ) where

import System.Environment
import System.IO
import Control.Monad

import Environment
import Parsers
import Evaluator

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

readEvalPrint :: EnvRef -> String -> IO ()
readEvalPrint envRef str = (eval envRef $ readExpr str) >>= putStrLn . show

loopUntil :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
loopUntil pred prompt action = do {
    input <- prompt;
    if pred input 
      then return ()
      else (action input >> loopUntil pred prompt action)
}

readEvalPrintLoop :: IO ()
readEvalPrintLoop = nullEnvRef >>=
                    loopUntil (== "quit") (readPrompt ">> ") . readEvalPrint
