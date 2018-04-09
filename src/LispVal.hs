module LispVal
    ( LispVal (..)
    ) where

data LispVal = Atom String
             | List [LispVal]
             | Number Integer

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ (unwords $ map show contents) ++ ")"
showVal (Number n) = show n
