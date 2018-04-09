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
  ("/", numBinOp div)
  ]

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numBinOp op args = Number $ foldl1 op $ map lvToInteger args

lvToInteger :: LispVal -> Integer
lvToInteger (Number n) = n
lvToInteger _          = 0
