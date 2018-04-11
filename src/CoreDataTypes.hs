module CoreDataTypes
    ( LispVal (..),
      LispError (..),
      EnvRef,
      nullEnvRef,
      Throwable, 
      IOThrowable, 
      liftThrowable, 
      runIOThrowable 
    ) where

import Control.Monad
import Control.Monad.Except
import System.IO
import Data.IORef
import Text.ParserCombinators.Parsec
 
-- LispVal
data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> Throwable LispVal)
             | Func { params :: [String], body :: [LispVal], closure :: EnvRef }

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ (unwords $ map show contents) ++ ")"
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params=p, body=b, closure=e}) = 
  "(lambda (" ++ unwords (map show p) ++ ") ...)"

-- LispError
data LispError = ParserError ParseError
               | UnboundVar String
               | BadFormError LispVal
               | NotFunction String
               | TypeMismatch String LispVal
               | NumArgError Integer [LispVal]

instance Show LispError where show = showError

showError :: LispError -> String
showError (ParserError err) = "Parser error at " ++ show err
showError (UnboundVar var) = "Unrecognized variable: " ++ var
showError (BadFormError form) = "Cannot evaluate form: " ++ show form
showError (NotFunction func) = "Unrecognized function: " ++ func
showError (TypeMismatch t v) = "Type mismatch: expected " ++ t ++ ", found " ++ show v
showError (NumArgError n args) = "Incorrect number of arguments: "
                                    ++ "expected " ++ show n 
                                    ++ ", found args " ++ (unwords $ map show args)


-- EnvRef
type EnvRef = IORef [(String, IORef LispVal)]

nullEnvRef :: IO EnvRef
nullEnvRef = newIORef []

-- Throwable/IOThrowable
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
