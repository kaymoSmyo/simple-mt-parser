module MTParser.Source where

class IsSource a where
    head :: (IsSource a) => a -> Char
    tail :: (IsSource a) => a -> a
    null :: (IsSource a) => a
    isNull :: (IsSource a) => a -> Bool
