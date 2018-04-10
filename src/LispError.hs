module LispError ( LispError (..) ) where

import Text.ParserCombinators.Parsec

data LispError = ParserError ParseError

instance Show LispError where show = showError

showError :: LispError -> String
showError (ParserError err) = "Parser error at " ++ show err
