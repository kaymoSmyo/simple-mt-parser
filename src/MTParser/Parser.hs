{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MTParser.Parser (runParser, Parser, item, ParseError (..), mkParseError) where

import Control.Applicative ( Alternative((<|>), empty) ) 
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put, state), State, runState)

data ParseError = Err String
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
    empty = throwError $ Err ""

mkParseError :: String -> ParseError
mkParseError = Err

runParser :: Parser a -> String -> (Either ParseError a, String)
runParser (P m) = runState (runExceptT m)

item :: Parser Char
item =
    get >>= \case
        [] -> throwError $ Err "Empty Input"
        (x : xs) -> put xs >> pure x