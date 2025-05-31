module Main (main) where

import ByteChar8Test (byteStringChar8ToolTest) -- Import tests from ByteStringChar8Test.hs
import StringTest (stringToolTest) -- Import tests from StringTest.hs
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (
    Counts (errors, failures),
    Test (TestList),
    runTestTT,
 )
import TextTest (textToolTest)

allTests :: Test
allTests =
    TestList
        [ stringToolTest
        , byteStringChar8ToolTest
        , textToolTest
        ]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure