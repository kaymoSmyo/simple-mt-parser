module Main (main) where

main :: IO ()
main = do putStrLn "no impl"

-- import Control.Applicative (Alternative, many, optional, (<|>))
-- import MTParser (Parser, char, runParser, sat, string)
-- import MTParser.Parser (ParseError(Err))

-- mySepBy :: Alternative f => f a -> f sep -> f [a]
-- mySepBy p sep = mySepBy1 p sep <|> pure []

-- mySepBy1 :: Alternative f => f a -> f sep -> f [a]
-- mySepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- -- csvField :: Parser String String
-- csvField = many sat (`notElem` ",")

-- csvLine :: Parser String [String]
-- csvLine = mySepBy csvField (char ',')

-- parseNewline :: Parser String ()
-- parseNewline = (string "\r\n" *> pure ()) <|> (char '\n' *> pure ())

-- csvDocument :: Parser String [[String]]
-- csvDocument = mySepBy csvLine parseNewline <* optional parseNewline

-- main :: IO ()
-- main = do
--     let testCSVs =
--             [ ("Simple CSV", "name,age,city\nAlice,30,New York\nBob,24,Paris")
--             , ("With empty fields", "name,age,city\nCharlie,,London\n,40,Berlin")
--             , ("Single line, multiple fields", "header1,header2")
--             , ("Single line, single field", "lonely_field")
--             , ("Empty input", "")
--             , ("Only newlines (LF)", "\n\n")
--             , ("Only newlines (CRLF)", "\r\n\r\n")
--             , ("Trailing LF newline", "a,b\nc,d\n")
--             , ("Trailing CRLF newline", "a,b\r\nc,d\r\n")
--             , ("Mixed newlines", "a,b\nc,d\r\ne,f")
--             , ("Line with empty field at end", "val1,val2,")
--             , ("Line with empty field at start", ",val2,val3")
--             , ("Line with only commas (3 empty fields)", ",,")
--             , ("Line with one comma (2 empty fields)", ",")
--             ]

--     mapM_ (\(desc, csvInput) -> do
--         putStrLn $ "\n--- Parsing: " ++ desc ++ " ---"
--         putStrLn $ "Input (escaped): " ++ show csvInput
--         case runParser csvDocument csvInput of
--             (Left (Err err), _) -> putStrLn $ "Error parsing CSV: " ++ err -- Assuming error is String
--             (Right result, rest) -> do
--                 putStrLn "Successfully parsed as:"
--                 mapM_ print result
--                 if null rest
--                 then putStrLn "Input fully consumed."
--                 else putStrLn $ "Remaining input: " ++ show rest
--         ) testCSVs
