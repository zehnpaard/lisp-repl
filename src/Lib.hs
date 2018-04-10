module Lib
    ( readEvalPrintLoop
    ) where

import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Except

import Environment
import Parsers
import Evaluator
import IOThrowable

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

readEvalPrint :: EnvRef -> String -> IO ()
readEvalPrint envRef str = do {
    output <- runIOThrowable (readExpr str >>= eval envRef >>= liftIO . return . show);
    putStrLn output;
}

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
