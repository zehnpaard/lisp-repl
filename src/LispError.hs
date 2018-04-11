module LispError ( LispError (..) ) where

import Text.ParserCombinators.Parsec

import LispVal

data LispError = ParserError ParseError
               | UnboundVar String

instance Show LispError where show = showError

showError :: LispError -> String
showError (ParserError err) = "Parser error at " ++ show err
showError (UnboundVar var) = "Unrecognized variable: " ++ var
