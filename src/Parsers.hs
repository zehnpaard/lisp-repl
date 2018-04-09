module Parsers
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec

import LispVal

readExpr :: String -> LispVal
readExpr input = case (parse parseExpr "lisp" input) of
  Left err  -> Atom $ "No match: " ++ show err
  Right val -> val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseList

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do {
    first <- (letter <|> symbol);
    rest <- many (letter <|> digit <|> symbol);
    return $ Atom (first:rest);
}

parseList :: Parser LispVal
parseList = do {
    char '(';
    xs <- sepBy parseExpr (skipMany1 space);
    char ')';
    return $ List xs;
}
