module MTParser.Tools () where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Data.Char (
    isAlpha,
    isAlphaNum,
    isDigit,
    isLower,
    isSpace,
    isUpper,
 )
import MTParser.Parser (Parser, item)

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
        then pure x
        else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = pure []
string (x : xs) = do
    _ <- char x
    _ <- string xs
    pure (x : xs)

ident :: Parser String
ident = do
    x <- some lower
    xs <- many alphanum
    return (x ++ xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    _ <- many (sat isSpace)
    return ()

int :: Parser Int
int =
    do
        _ <- char '-'
        n <- nat
        return (-n)
        <|> nat

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v