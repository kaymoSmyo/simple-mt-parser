module ByteChar8Test (byteStringChar8ToolTest) where

import Data.ByteString.Char8 qualified as BS
import MTParser.Parser (ParseError (Err), runParser)

-- Import specific parsers or all from MTParser.Tools
import MTParser.Tools (
    alphanum,
    char,
    digit,
    ident,
    int,
    letter,
    lower,
    nat,
    space,
    string,
    token,
    upper,
 )
import Test.HUnit (
    Test (TestList),
    (~:),
    (~?=),
 )
import qualified Data.ByteString.Char8 as BS

byteStringChar8ToolTest :: Test
byteStringChar8ToolTest =
    TestList
        [ digitTest
        , lowerTest
        , upperTest
        , letterTest
        , alphanumTest
        , charTest
        , stringTest
        , identTest
        , natTest
        , spaceTest
        , intTest
        , tokenTest
        ]

-- Helper for expected results
success :: (Show a, Eq a) => a -> BS.ByteString -> (Either ParseError a, BS.ByteString)
success val rest = (Right val, rest)

-- Specific error message and remaining string at the point of error
expectError :: (Show a, Eq a) => String -> BS.ByteString -> (Either ParseError a, BS.ByteString)
expectError msg expectedRest = (Left (Err msg), expectedRest)

-- Default error from runParser when a parser yields no result (returns [])
-- The argument is the expected remaining string when this error occurs.
expectRunParserEmpty :: (Show a, Eq a) => BS.ByteString -> (Either ParseError a, BS.ByteString)
expectRunParserEmpty remaining = (Left (Err "Empty Input"), remaining)

-- Tests for each parser from MTParser.Tools

digitTest :: Test
digitTest =
    "digit (ByteString)"
        ~: TestList
            [ "parse '1'" ~: runParser digit (BS.pack "1") ~?= success '1' (BS.pack "")
            , "parse '7' with rest" ~: runParser digit (BS.pack "7abc") ~?= success '7' (BS.pack "abc")
            , "fail on space then '1'" ~: runParser digit (BS.pack " 1") ~?= expectError "not predicate at  " (BS.pack "1")
            , "fail on 'a'" ~: runParser digit (BS.pack "a") ~?= expectError "not predicate at a" (BS.pack "")
            , "fail on 'a' with rest" ~: runParser digit (BS.pack "abc") ~?= expectError "not predicate at a" (BS.pack "bc")
            , "fail on empty for digit" ~: runParser digit (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

lowerTest :: Test
lowerTest =
    "lower (ByteString)"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser lower (BS.pack "a") ~?= success 'a' (BS.pack "")
            , "parse lowercase 'z' with rest" ~: runParser lower (BS.pack "zXY") ~?= success 'z' (BS.pack "XY")
            , "fail on uppercase 'A'" ~: runParser lower (BS.pack "A") ~?= expectError "not predicate at A" (BS.pack "")
            , "fail on uppercase 'B' with rest" ~: runParser lower (BS.pack "BC") ~?= expectError "not predicate at B" (BS.pack "C")
            , "fail on digit '1'" ~: runParser lower (BS.pack "1") ~?= expectError "not predicate at 1" (BS.pack "")
            , "fail on empty" ~: runParser lower (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

upperTest :: Test
upperTest =
    "upper (ByteString)"
        ~: TestList
            [ "parse uppercase 'A'" ~: runParser upper (BS.pack "A") ~?= success 'A' (BS.pack "")
            , "parse uppercase 'Z' with rest" ~: runParser upper (BS.pack "Zxy") ~?= success 'Z' (BS.pack "xy")
            , "fail on lowercase 'a'" ~: runParser upper (BS.pack "a") ~?= expectError "not predicate at a" (BS.pack "")
            , "fail on lowercase 'b' with rest" ~: runParser upper (BS.pack "bc") ~?= expectError "not predicate at b" (BS.pack "c")
            , "fail on digit '1'" ~: runParser upper (BS.pack "1") ~?= expectError "not predicate at 1" (BS.pack "")
            , "fail on empty" ~: runParser upper (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

letterTest :: Test
letterTest =
    "letter (ByteString)"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser letter (BS.pack "a") ~?= success 'a' (BS.pack "")
            , "parse uppercase 'A'" ~: runParser letter (BS.pack "A") ~?= success 'A' (BS.pack "")
            , "parse letter 'x' with rest" ~: runParser letter (BS.pack "x123") ~?= success 'x' (BS.pack "123")
            , "fail on digit '1'" ~: runParser letter (BS.pack "1") ~?= expectError "not predicate at 1" (BS.pack "")
            , "fail on symbol '*'" ~: runParser letter (BS.pack "*") ~?= expectError "not predicate at *" (BS.pack "")
            , "fail on empty" ~: runParser letter (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

alphanumTest :: Test
alphanumTest =
    "alphanum (ByteString)"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser alphanum (BS.pack "a") ~?= success 'a' (BS.pack "")
            , "parse uppercase 'A'" ~: runParser alphanum (BS.pack "A") ~?= success 'A' (BS.pack "")
            , "parse digit '7'" ~: runParser alphanum (BS.pack "7") ~?= success '7' (BS.pack "")
            , "parse alphanum 'Z' with rest" ~: runParser alphanum (BS.pack "Z==") ~?= success 'Z' (BS.pack "==")
            , "fail on symbol '-'" ~: runParser alphanum (BS.pack "-") ~?= expectError "not predicate at -" (BS.pack "")
            , "fail on empty" ~: runParser alphanum (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

charTest :: Test
charTest =
    "char (ByteString)"
        ~: TestList
            [ "parse matching char 'x'" ~: runParser (char 'x') (BS.pack "x") ~?= success 'x' (BS.pack "")
            , "parse matching char 'y' with rest" ~: runParser (char 'y') (BS.pack "yz") ~?= success 'y' (BS.pack "z")
            , "fail on non-matching char 'x' for 'y'" ~: runParser (char 'x') (BS.pack "y") ~?= expectError "not predicate at y" (BS.pack "")
            , "fail on non-matching char 'a' for 'b' with rest" ~: runParser (char 'a') (BS.pack "bc") ~?= expectError "not predicate at b" (BS.pack "c")
            , "fail on empty for char 'x'" ~: runParser (char 'x') (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

stringTest :: Test
stringTest =
    "string (ByteString)"
        ~: TestList
            [ "parse matching string \"abc\"" ~: runParser (string "abc") (BS.pack "abc") ~?= success "abc" (BS.pack "")
            , "parse matching string \"hello\" with rest" ~: runParser (string "hello") (BS.pack "helloworld") ~?= success "hello" (BS.pack "world")
            , "fail on partial match \"ab\" for \"abc\"" ~: runParser (string "abc") (BS.pack "ab") ~?= expectRunParserEmpty (BS.pack "") -- Assumes parser consumes "ab", then fails on "" for 'c'
            , "fail on mismatch \"axc\" for \"abc\"" ~: runParser (string "abc") (BS.pack "axc") ~?= expectError "not predicate at x" (BS.pack "c")
            , "parse empty string" ~: runParser (string "") (BS.pack "abc") ~?= success "" (BS.pack "abc")
            , "parse empty string on empty input" ~: runParser (string "") (BS.pack "") ~?= success "" (BS.pack "")
            , "fail on non-empty string \"a\" on empty input" ~: runParser (string "a") (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

identTest :: Test
identTest =
    "ident (ByteString)"
        ~: TestList
            [ "parse single lower 'a'" ~: runParser ident (BS.pack "a") ~?= success "a" (BS.pack "")
            , "parse lower then alphanum 'a1'" ~: runParser ident (BS.pack "a1") ~?= success "a1" (BS.pack "")
            , "parse multiple lower then multiple alphanum 'abc123xyz'" ~: runParser ident (BS.pack "abc123xyz") ~?= success "abc123xyz" (BS.pack "")
            , "parse only 'lower' case" ~: runParser ident (BS.pack "lower") ~?= success "lower" (BS.pack "")
            , "stop at non-alphanum 'ident-'" ~: runParser ident (BS.pack "ident-") ~?= success "ident" (BS.pack "-")
            , "parse 'aBc' as 'aBc'" ~: runParser ident (BS.pack "aBc") ~?= success "aBc" (BS.pack "")
            , "parse 'camelCase'" ~: runParser ident (BS.pack "camelCase") ~?= success "camelCase" (BS.pack "")
            , "fail on starting with digit '1a'" ~: runParser ident (BS.pack "1a") ~?= expectError "not predicate at 1" (BS.pack "a")
            , "fail on starting with uppercase 'Ident'" ~: runParser ident (BS.pack "Ident") ~?= expectError "not predicate at I" (BS.pack "dent")
            , "fail on starting with symbol '_ident'" ~: runParser ident (BS.pack "_ident") ~?= expectError "not predicate at _" (BS.pack "ident")
            , "fail on empty for ident" ~: runParser ident (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

natTest :: Test
natTest =
    "nat (ByteString)"
        ~: TestList
            [ "parse positive number 123" ~: runParser nat (BS.pack "123") ~?= success 123 (BS.pack "")
            , "parse zero 0" ~: runParser nat (BS.pack "0") ~?= success 0 (BS.pack "")
            , "parse number 123 with trailing 'a'" ~: runParser nat (BS.pack "123a") ~?= success 123 (BS.pack "a")
            , "fail on non-digit start 'a123'" ~: runParser nat (BS.pack "a123") ~?= expectError "not predicate at a" (BS.pack "123")
            , "fail on empty for nat" ~: runParser nat (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            , "parse single digit 5" ~: runParser nat (BS.pack "5") ~?= success 5 (BS.pack "")
            ]

spaceTest :: Test
spaceTest =
    "space (ByteString)"
        ~: TestList
            [ "consume multiple spaces '   '" ~: runParser space (BS.pack "   ") ~?= success () (BS.pack "")
            , "consume spaces before text '   abc'" ~: runParser space (BS.pack "   abc") ~?= success () (BS.pack "abc")
            , "consume no spaces if none present 'abc'" ~: runParser space (BS.pack "abc") ~?= success () (BS.pack "abc")
            , "consume no spaces on empty input ''" ~: runParser space (BS.pack "") ~?= success () (BS.pack "")
            ]

intTest :: Test
intTest =
    "int (ByteString)"
        ~: TestList
            [ "parse positive int 123" ~: runParser int (BS.pack "123") ~?= success 123 (BS.pack "")
            , "parse negative int -123" ~: runParser int (BS.pack "-123") ~?= success (-123) (BS.pack "")
            , "parse zero 0" ~: runParser int (BS.pack "0") ~?= success 0 (BS.pack "")
            , "parse negative zero -0" ~: runParser int (BS.pack "-0") ~?= success 0 (BS.pack "") -- -0 is 0
            , "parse positive int 123 with rest" ~: runParser int (BS.pack "123a") ~?= success 123 (BS.pack "a")
            , "parse negative int -123 with rest" ~: runParser int (BS.pack "-123a") ~?= success (-123) (BS.pack "a")
            , "fail on non-digit start 'a123'" ~: runParser int (BS.pack "a123") ~?= expectError "not predicate at a" (BS.pack "123")
            , "fail on '-' then non-digit '-a123'" ~: runParser int (BS.pack "-a123") ~?= expectError "not predicate at -" (BS.pack "a123")
            , "fail on just '-'" ~: runParser int (BS.pack "-") ~?= expectError "not predicate at -" (BS.pack "")
            , "fail on empty for int" ~: runParser int (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            ]

tokenTest :: Test
tokenTest =
    "token (ByteString)"
        ~: TestList
            [ "tokenize digit '1'" ~: runParser (token digit) (BS.pack "1") ~?= success '1' (BS.pack "")
            , "tokenize digit ' 1 '" ~: runParser (token digit) (BS.pack " 1 ") ~?= success '1' (BS.pack "")
            , "tokenize digit '  1'" ~: runParser (token digit) (BS.pack "  1") ~?= success '1' (BS.pack "")
            , "tokenize digit '1  '" ~: runParser (token digit) (BS.pack "1  ") ~?= success '1' (BS.pack "")
            , "tokenize digit '  123'" ~: runParser (token digit) (BS.pack "  123") ~?= success '1' (BS.pack "23")
            , "tokenize digit '1   abc'" ~: runParser (token digit) (BS.pack "1   abc") ~?= success '1' (BS.pack "abc")
            , "tokenize string \"word\" with spaces" ~: runParser (token (string "word")) (BS.pack "  word  ") ~?= success "word" (BS.pack "")
            , "tokenize nat with spaces" ~: runParser (token nat) (BS.pack "  123  ") ~?= success 123 (BS.pack "")
            , "fail token nat on '  -123  '" ~: runParser (token nat) (BS.pack "  -123  ") ~?= expectError "not predicate at -" (BS.pack "123  ")
            , "fail token digit on ' abc '" ~: runParser (token digit) (BS.pack " abc ") ~?= expectError "not predicate at a" (BS.pack "bc ")
            , "token on empty input with parser that succeeds on empty (like string \"\")" ~: runParser (token (string "")) (BS.pack "") ~?= success "" (BS.pack "")
            , "token on empty input with parser that fails on empty (like digit)" ~: runParser (token digit) (BS.pack "") ~?= expectRunParserEmpty (BS.pack "")
            , "token on spaces only with parser that fails on empty (like digit)" ~: runParser (token digit) (BS.pack "   ") ~?= expectRunParserEmpty (BS.pack "")
            ]
