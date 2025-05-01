module Main (main) where

import MTParser

main :: IO ()
main = putStrLn "Test suite not yet implemented."

runTest ::
    (Eq a, Show a) =>
    Parser a ->
    String ->
    (Either ParseError a, String) ->
    IO ()
runTest p str ret = do
    let t = runParser p str
    if t /= ret
        then
            mapM_
                putStrLn
                [ "fail: " ++ str
                , "Expected: " ++ show ret
                , "actual: " ++ show t
                ]
        else
            putStrLn "success"
    return ()