module LispVal
    ( LispVal (..)
    ) where

data LispVal = Atom String

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
