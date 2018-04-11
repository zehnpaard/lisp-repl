module Functions
    ( apply
    ) where

import Control.Monad.Except

import LispVal
import LispError
import IOThrowable

apply :: String -> [LispVal] -> Throwable LispVal
apply funcName args = maybe (throwError $ NotFunction funcName) 
                            ($ args)
                            (lookup funcName primitives)

primitives :: [(String, [LispVal] -> Throwable LispVal)]
primitives = [
  ("+", numBinOp (+)),
  ("-", numBinOp (-)),
  ("*", numBinOp (*)),
  ("/", numBinOp div),
  ("=", numBoolBinOp (==)),
  ("/=", numBoolBinOp (/=)),
  (">", numBoolBinOp (>)),
  (">=", numBoolBinOp (>=)),
  ("<", numBoolBinOp (<)),
  ("<=", numBoolBinOp (<=)),
  ("and", boolBoolBinOp (&&)),
  ("or", boolBoolBinOp (||))
  ]

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> Throwable LispVal
numBinOp op args = mapM lvToInteger args >>= return . Number . foldl1 op 

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> Throwable LispVal
numBoolBinOp op [x, y] = do {
    a <- lvToInteger x;
    b <- lvToInteger y;
    return $ Bool $ op a b;
}
numBoolBinOp op args   = throwError $ NumArgError 2 args

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> Throwable LispVal
boolBoolBinOp op args = mapM lvToBool args >>= return . Bool . foldl1 op 

lvToInteger :: LispVal -> Throwable Integer
lvToInteger (Number n) = return n
lvToInteger notNumber  = throwError $ TypeMismatch "Number" notNumber

lvToBool :: LispVal -> Throwable Bool
lvToBool (Bool b) = return b
lvToBool notBool  = throwError $ TypeMismatch "Bool" notBool
