module LispVal
    ( LispVal (..)
    ) where

data LispVal = Atom String
             | List [LispVal]

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ (unwords $ map show contents) ++ ")"
