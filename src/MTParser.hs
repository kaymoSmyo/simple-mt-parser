{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MTParser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put, state), State, runState)

data ParseError = InvalidInput String | EmptyInput String
    deriving (Show, Eq)

newtype Parser a = P (ExceptT ParseError (State String) a)
    deriving (Functor, Applicative, Monad, MonadError ParseError, MonadState String)

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
