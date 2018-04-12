module Parsers
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec
import Control.Monad.Except

import CoreDataTypes

readExpr :: String -> IOThrowable LispVal
readExpr input = case (parse parseExpr "lisp" input) of
  Left err  -> throwError $ ParserError err
  Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseBool
        <|> parseAtom
        <|> parseListType
        <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do {
    first <- (letter <|> symbol);
    rest <- many (letter <|> digit <|> symbol);
    return $ Atom (first:rest);
}

parseListType :: Parser LispVal
parseListType = do {
    char '(';
    list <- (try parseDottedList <|> parseList);
    char ')';
    return list;
}

parseDottedList :: Parser LispVal
parseDottedList = do {
    head <- endBy parseExpr (skipMany1 space);
    char '.';
    skipMany1 space;
    tail <- parseExpr;
    return $ DottedList head tail;
}

parseList :: Parser LispVal
parseList = do {
    xs <- sepBy parseExpr (skipMany1 space);
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
