module Functions
    ( apply
    ) where

import LispVal

apply :: String -> [LispVal] -> LispVal
apply funcName args = maybe (Number 0) ($ args) (lookup funcName primitives)

primitives :: [(String, [LispVal] -> LispVal)]
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

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numBinOp op args = Number $ foldl1 op $ map lvToInteger args

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
numBoolBinOp op [x, y] = Bool $ op (lvToInteger x) (lvToInteger y)
numBoolBinOp op _      = Bool False

boolBoolBinOp op args = Bool $ foldl1 op $ map lvToBool args

lvToInteger :: LispVal -> Integer
lvToInteger (Number n) = n
lvToInteger _          = 0

lvToBool :: LispVal -> Bool
lvToBool (Bool b) = b
lvToBool _        = False
