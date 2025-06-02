module Main (main) where

import Control.Applicative
import MTParser

main :: IO ()
main = do putStrLn "not impl"

field :: Parser String String
field = some alphanum

connma :: Parser String Char
connma = char ','

record :: Parser String [String]
record = (:) <$> field <*> many (connma *> field)
