{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MTParser.Parser (runParser, Parser, item, ParseError (..), mkParseError) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put, state), State, runState)
import MTParser.Source qualified as S

data ParseError = Err String
    deriving (Show, Eq)

newtype Parser s a = P (ExceptT ParseError (State s) a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError ParseError
        , MonadState s
        )

instance (S.IsSource s) => Alternative (Parser s) where
    (<|>) :: Parser s a -> Parser s a -> Parser s a
    p <|> q = P $ ExceptT $ state $ \input ->
        case runParser p input of
            (Left _, _) -> runParser q input
            ok -> ok
    empty :: Parser s a
    empty = throwError $ Err ""

mkParseError :: String -> ParseError
mkParseError = Err

runParser :: (S.IsSource s) => Parser s a -> s -> (Either ParseError a, s)
runParser (P m) = runState (runExceptT m)

item :: (S.IsSource s) => Parser s Char
item = do
    s <- get
    if S.isNull s
        then throwError $ Err "Empty Input"
        else
            let x = S.head s
                xs = S.tail s
             in put xs >> pure x
