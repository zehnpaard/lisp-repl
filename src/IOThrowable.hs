module IOThrowable ( Throwable, IOThrowable, liftThrowable, runIOThrowable ) where

import Control.Monad
import Control.Monad.Except
import System.IO

import LispError

type Throwable   = Either LispError
type IOThrowable = ExceptT LispError IO

liftThrowable :: Throwable a -> IOThrowable a
liftThrowable (Left err) = throwError err
liftThrowable (Right val) = return val

trapError :: IOThrowable String -> IOThrowable String
trapError action = catchError action (return . show)

extractValue :: Either a String -> String
extractValue (Right val) = val

runIOThrowable :: IOThrowable String -> IO String
runIOThrowable action = runExceptT (trapError action) >>= return . extractValue
