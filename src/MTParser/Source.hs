module MTParser.Source where

import Data.ByteString.Char8 qualified as C
import Data.List qualified as S
import Data.Text qualified as T

import Data.Maybe (fromJust)

class IsSource a where
    head :: (IsSource a) => a -> Char
    tail :: (IsSource a) => a -> a
    null :: (IsSource a) => a
    isNull :: (IsSource a) => a -> Bool

instance IsSource String where
    head :: (IsSource String) => String -> Char
    head = S.head
    tail :: (IsSource String) => String -> String
    tail = S.tail
    null :: (IsSource String) => String
    null = ""
    isNull :: (IsSource String) => String -> Bool
    isNull = S.null

instance IsSource C.ByteString where
    head :: (IsSource C.ByteString) => C.ByteString -> Char
    head = fst . fromJust . C.uncons
    tail :: (IsSource C.ByteString) => C.ByteString -> C.ByteString
    tail = fst . fromJust . C.unsnoc
    null :: (IsSource C.ByteString) => C.ByteString
    null = C.empty
    isNull :: (IsSource C.ByteString) => C.ByteString -> Bool
    isNull = C.null

instance IsSource T.Text where
    head :: (IsSource T.Text) => T.Text -> Char
    head = fst . fromJust . T.uncons
    tail :: (IsSource T.Text) => T.Text -> T.Text
    tail = fst . fromJust . T.unsnoc
    null :: (IsSource T.Text) => T.Text
    null = T.empty
    isNull :: (IsSource T.Text) => T.Text -> Bool
    isNull = T.null