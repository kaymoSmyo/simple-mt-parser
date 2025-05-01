{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MTParser where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put, state), State, runState)
import Data.Char

data ParseError = InvalidInput String | EmptyInput String
    deriving (Show, Eq)

newtype Parser a = P (ExceptT ParseError (State String) a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError ParseError
        , MonadState String
        )

instance Alternative Parser where
    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ ExceptT $ state $ \input ->
        case runParser p input of
            (Left _, _) -> runParser q input
            ok -> ok
    empty :: Parser a
    empty = throwError $ EmptyInput "empty input"

runParser :: Parser a -> String -> (Either ParseError a, String)
runParser (P m) = runState (runExceptT m)

item :: Parser Char
item =
    get >>= \input ->
        case input of
            [] -> throwError $ EmptyInput input
            (x : xs) -> put xs >> pure x

atThree :: Parser Char
atThree = let f _ _ z = z in f <$> item <*> item <*> item

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
int = do
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