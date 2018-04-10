module Parsers
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec
import Control.Monad.Except

import LispVal
import LispError
import IOThrowable

readExpr :: String -> IOThrowable LispVal
readExpr input = case (parse parseExpr "lisp" input) of
  Left err  -> throwError $ ParserError err
  Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseBool
        <|> parseAtom
        <|> parseList
        <|> parseNumber

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

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

parseBool :: Parser LispVal
parseBool = (try parseTrue) <|> (try parseFalse)

parseTrue :: Parser LispVal
parseTrue = string "#t" >> return (Bool True)

parseFalse :: Parser LispVal
parseFalse = string "#f" >> return (Bool False)
