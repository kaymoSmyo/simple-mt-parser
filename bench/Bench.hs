module Bench () where

-- import Criterion.Main
-- import MTParser.Parser (ParseError, Parser, runParser)
-- import MTParser.Source qualified as S
-- import MTParser.Tools (letter)

-- import Data.ByteString.Char8 (ByteString)
-- import Data.ByteString.Char8 qualified as BS
-- import Data.Text (Text)
-- import Data.Text qualified as T

-- -- ベンチマーク対象のパーサー
-- -- 多数の文字をパースし、文字列として返す
-- parserToBench :: S.IsSource s => Parser s String
-- parserToBench = many letter

-- -- 入力データ
-- longStringLength :: Int
-- longStringLength = 100000 -- ベンチマークに適した長さの文字列

-- -- 簡単のため 'a' を繰り返す文字列を使用
-- -- より現実的なベンチマークのためには、代表的なテキストデータを使用してください
-- longStringInput :: String
-- longStringInput = replicate longStringLength 'a'

-- longTextInput :: Text
-- longTextInput = T.pack longStringInput

-- longByteStringInput :: ByteString
-- longByteStringInput = BS.pack longStringInput

-- main :: IO ()
-- main = defaultMain [
--     bgroup "many letter parser performance" [
--         bench "String input"     $ nf (runParser parserToBench) longStringInput,
--         bench "Text input"       $ nf (runParser parserToBench) longTextInput,
--         bench "ByteString input" $ nf (runParser parserToBench) longByteStringInput
--     ]
--   ]