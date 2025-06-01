module MTParser.Tools where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Char (
    isAlpha,
    isAlphaNum,
    isDigit,
    isLower,
    isSpace,
    isUpper,
 )
import Data.Functor (($>))
import MTParser.Parser (ParseError (Err), Parser, item)
import MTParser.Source qualified as S

sat :: (S.IsSource s) => (Char -> Bool) -> Parser s Char
sat p = do
    x <- item
    if p x
        then pure x
        else throwError . Err $ "not predicate at " ++ [x]

digit :: (S.IsSource s) => Parser s Char
digit = sat isDigit

lower :: (S.IsSource s) => Parser s Char
lower = sat isLower

upper :: (S.IsSource s) => Parser s Char
upper = sat isUpper

letter :: (S.IsSource s) => Parser s Char
letter = sat isAlpha

alphanum :: (S.IsSource s) => Parser s Char
alphanum = sat isAlphaNum

char :: (S.IsSource s) => Char -> Parser s Char
char x = sat (== x)

string :: (S.IsSource s) => String -> Parser s String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure "")

ident :: (S.IsSource s) => Parser s String
ident = (++) <$> some lower <*> many alphanum

nat :: (S.IsSource s) => Parser s Int
nat = read <$> some digit

space :: (S.IsSource s) => Parser s ()
space = many (sat isSpace) Data.Functor.$> ()

int :: (S.IsSource s) => Parser s Int
int = (negate <$> (char '-' *> nat)) <|> nat

token :: (S.IsSource s) => Parser s a -> Parser s a
token p = space *> p <* space
