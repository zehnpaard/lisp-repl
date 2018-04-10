module IOThrowable ( IOThrowable, runIOThrowable ) where

import Control.Monad
import Control.Monad.Except
import System.IO

import LispError

type IOThrowable = ExceptT LispError IO

trapError :: IOThrowable String -> IOThrowable String
trapError action = catchError action (return . show)

extractValue :: Either a String -> String
extractValue (Right val) = val

runIOThrowable :: IOThrowable String -> IO String
runIOThrowable action = runExceptT (trapError action) >>= return . extractValue
