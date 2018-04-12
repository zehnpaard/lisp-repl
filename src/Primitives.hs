module Primitives (primitiveBindings) where

import Control.Monad.Except

import CoreDataTypes
import Variables

primitiveBindings :: IO EnvRef
primitiveBindings = do 
    let f (var, func) = (var, PrimitiveFunc func)
    let pfs = map f primitives
    envRef <- nullEnvRef
    bindVars envRef pfs

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
  ("or", boolBoolBinOp (||)),
  ("car", car),
  ("cdr", cdr),
  ("cons", cons),
  ("eqv?", eqv)
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

car :: [LispVal] -> Throwable LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [nonListType] = throwError $ TypeMismatch "Non-empty ListType" nonListType
car badArgs = throwError $ NumArgError 1 badArgs

cdr :: [LispVal] -> Throwable LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] y] = return y
cdr [DottedList (x:xs) y] = return $ DottedList xs y
cdr [nonListType] = throwError $ TypeMismatch "Non-empty ListType" nonListType
cdr badArgs = throwError $ NumArgError 1 badArgs

cons :: [LispVal] -> Throwable LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
cons badArgs = throwError $ NumArgError 2 badArgs

eqv :: [LispVal] -> Throwable LispVal
eqv xs = return $ Bool (and (zipWith (==) ys (tail ys)))
  where ys = map show xs
