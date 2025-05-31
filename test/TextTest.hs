--- /dev/null
module TextTest (textToolTest) where

import Data.Text qualified as T
import MTParser.Parser (ParseError (Err), runParser)
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

textToolTest :: Test
textToolTest =
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
success :: (Show a, Eq a) => a -> T.Text -> (Either ParseError a, T.Text)
success val rest = (Right val, rest)

-- Specific error message and remaining string at the point of error
expectError :: (Show a, Eq a) => String -> T.Text -> (Either ParseError a, T.Text)
expectError msg expectedRest = (Left (Err msg), expectedRest)

-- Default error from runParser when a parser yields no result (returns [])
-- The argument is the expected remaining string when this error occurs.
expectRunParserEmpty :: (Show a, Eq a) => T.Text -> (Either ParseError a, T.Text)
expectRunParserEmpty remaining = (Left (Err "Empty Input"), remaining)

-- Tests for each parser from MTParser.Tools

digitTest :: Test
digitTest =
    "digit (Text)"
        ~: TestList
            [ "parse '1'" ~: runParser digit (T.pack "1") ~?= success '1' (T.pack "")
            , "parse '7' with rest" ~: runParser digit (T.pack "7abc") ~?= success '7' (T.pack "abc")
            , "fail on space then '1'" ~: runParser digit (T.pack " 1") ~?= expectError "not predicate at  " (T.pack "1")
            , "fail on 'a'" ~: runParser digit (T.pack "a") ~?= expectError "not predicate at a" (T.pack "")
            , "fail on 'a' with rest" ~: runParser digit (T.pack "abc") ~?= expectError "not predicate at a" (T.pack "bc")
            , "fail on empty for digit" ~: runParser digit (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

lowerTest :: Test
lowerTest =
    "lower (Text)"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser lower (T.pack "a") ~?= success 'a' (T.pack "")
            , "parse lowercase 'z' with rest" ~: runParser lower (T.pack "zXY") ~?= success 'z' (T.pack "XY")
            , "fail on uppercase 'A'" ~: runParser lower (T.pack "A") ~?= expectError "not predicate at A" (T.pack "")
            , "fail on uppercase 'B' with rest" ~: runParser lower (T.pack "BC") ~?= expectError "not predicate at B" (T.pack "C")
            , "fail on digit '1'" ~: runParser lower (T.pack "1") ~?= expectError "not predicate at 1" (T.pack "")
            , "fail on empty" ~: runParser lower (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

upperTest :: Test
upperTest =
    "upper (Text)"
        ~: TestList
            [ "parse uppercase 'A'" ~: runParser upper (T.pack "A") ~?= success 'A' (T.pack "")
            , "parse uppercase 'Z' with rest" ~: runParser upper (T.pack "Zxy") ~?= success 'Z' (T.pack "xy")
            , "fail on lowercase 'a'" ~: runParser upper (T.pack "a") ~?= expectError "not predicate at a" (T.pack "")
            , "fail on lowercase 'b' with rest" ~: runParser upper (T.pack "bc") ~?= expectError "not predicate at b" (T.pack "c")
            , "fail on digit '1'" ~: runParser upper (T.pack "1") ~?= expectError "not predicate at 1" (T.pack "")
            , "fail on empty" ~: runParser upper (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

letterTest :: Test
letterTest =
    "letter (Text)"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser letter (T.pack "a") ~?= success 'a' (T.pack "")
            , "parse uppercase 'A'" ~: runParser letter (T.pack "A") ~?= success 'A' (T.pack "")
            , "parse letter 'x' with rest" ~: runParser letter (T.pack "x123") ~?= success 'x' (T.pack "123")
            , "fail on digit '1'" ~: runParser letter (T.pack "1") ~?= expectError "not predicate at 1" (T.pack "")
            , "fail on symbol '*'" ~: runParser letter (T.pack "*") ~?= expectError "not predicate at *" (T.pack "")
            , "fail on empty" ~: runParser letter (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

alphanumTest :: Test
alphanumTest =
    "alphanum (Text)"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser alphanum (T.pack "a") ~?= success 'a' (T.pack "")
            , "parse uppercase 'A'" ~: runParser alphanum (T.pack "A") ~?= success 'A' (T.pack "")
            , "parse digit '7'" ~: runParser alphanum (T.pack "7") ~?= success '7' (T.pack "")
            , "parse alphanum 'Z' with rest" ~: runParser alphanum (T.pack "Z==") ~?= success 'Z' (T.pack "==")
            , "fail on symbol '-'" ~: runParser alphanum (T.pack "-") ~?= expectError "not predicate at -" (T.pack "")
            , "fail on empty" ~: runParser alphanum (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

charTest :: Test
charTest =
    "char (Text)"
        ~: TestList
            [ "parse matching char 'x'" ~: runParser (char 'x') (T.pack "x") ~?= success 'x' (T.pack "")
            , "parse matching char 'y' with rest" ~: runParser (char 'y') (T.pack "yz") ~?= success 'y' (T.pack "z")
            , "fail on non-matching char 'x' for 'y'" ~: runParser (char 'x') (T.pack "y") ~?= expectError "not predicate at y" (T.pack "")
            , "fail on non-matching char 'a' for 'b' with rest" ~: runParser (char 'a') (T.pack "bc") ~?= expectError "not predicate at b" (T.pack "c")
            , "fail on empty for char 'x'" ~: runParser (char 'x') (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

stringTest :: Test
stringTest =
    "string (Text)"
        ~: TestList
            [ "parse matching string \"abc\"" ~: runParser (string "abc") (T.pack "abc") ~?= success "abc" (T.pack "")
            , "parse matching string \"hello\" with rest" ~: runParser (string "hello") (T.pack "helloworld") ~?= success "hello" (T.pack "world")
            , "fail on partial match \"ab\" for \"abc\"" ~: runParser (string "abc") (T.pack "ab") ~?= expectRunParserEmpty (T.pack "") -- Assumes parser consumes "ab", then fails on "" for 'c'
            , "fail on mismatch \"axc\" for \"abc\"" ~: runParser (string "abc") (T.pack "axc") ~?= expectError "not predicate at x" (T.pack "c")
            , "parse empty string" ~: runParser (string "") (T.pack "abc") ~?= success "" (T.pack "abc")
            , "parse empty string on empty input" ~: runParser (string "") (T.pack "") ~?= success "" (T.pack "")
            , "fail on non-empty string \"a\" on empty input" ~: runParser (string "a") (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

identTest :: Test
identTest =
    "ident (Text)"
        ~: TestList
            [ "parse single lower 'a'" ~: runParser ident (T.pack "a") ~?= success "a" (T.pack "")
            , "parse lower then alphanum 'a1'" ~: runParser ident (T.pack "a1") ~?= success "a1" (T.pack "")
            , "parse multiple lower then multiple alphanum 'abc123xyz'" ~: runParser ident (T.pack "abc123xyz") ~?= success "abc123xyz" (T.pack "")
            , "parse only 'lower' case" ~: runParser ident (T.pack "lower") ~?= success "lower" (T.pack "")
            , "stop at non-alphanum 'ident-'" ~: runParser ident (T.pack "ident-") ~?= success "ident" (T.pack "-")
            , "parse 'aBc' as 'aBc'" ~: runParser ident (T.pack "aBc") ~?= success "aBc" (T.pack "")
            , "parse 'camelCase'" ~: runParser ident (T.pack "camelCase") ~?= success "camelCase" (T.pack "")
            , "fail on starting with digit '1a'" ~: runParser ident (T.pack "1a") ~?= expectError "not predicate at 1" (T.pack "a")
            , "fail on starting with uppercase 'Ident'" ~: runParser ident (T.pack "Ident") ~?= expectError "not predicate at I" (T.pack "dent")
            , "fail on starting with symbol '_ident'" ~: runParser ident (T.pack "_ident") ~?= expectError "not predicate at _" (T.pack "ident")
            , "fail on empty for ident" ~: runParser ident (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

natTest :: Test
natTest =
    "nat (Text)"
        ~: TestList
            [ "parse positive number 123" ~: runParser nat (T.pack "123") ~?= success (123 :: Int) (T.pack "")
            , "parse zero 0" ~: runParser nat (T.pack "0") ~?= success 0 (T.pack "")
            , "parse number 123 with trailing 'a'" ~: runParser nat (T.pack "123a") ~?= success 123 (T.pack "a")
            , "fail on non-digit start 'a123'" ~: runParser nat (T.pack "a123") ~?= expectError "not predicate at a" (T.pack "123")
            , "fail on empty for nat" ~: runParser nat (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            , "parse single digit 5" ~: runParser nat (T.pack "5") ~?= success 5 (T.pack "")
            ]

spaceTest :: Test
spaceTest =
    "space (Text)"
        ~: TestList
            [ "consume multiple spaces '   '" ~: runParser space (T.pack "   ") ~?= success () (T.pack "")
            , "consume spaces before text '   abc'" ~: runParser space (T.pack "   abc") ~?= success () (T.pack "abc")
            , "consume no spaces if none present 'abc'" ~: runParser space (T.pack "abc") ~?= success () (T.pack "abc")
            , "consume no spaces on empty input ''" ~: runParser space (T.pack "") ~?= success () (T.pack "")
            ]

intTest :: Test
intTest =
    "int (Text)"
        ~: TestList
            [ "parse positive int 123" ~: runParser int (T.pack "123") ~?= success (123 :: Int) (T.pack "")
            , "parse negative int -123" ~: runParser int (T.pack "-123") ~?= success ((-123) :: Int) (T.pack "")
            , "parse zero 0" ~: runParser int (T.pack "0") ~?= success 0 (T.pack "")
            , "parse negative zero -0" ~: runParser int (T.pack "-0") ~?= success (0 :: Int) (T.pack "") -- -0 is 0
            , "parse positive int 123 with rest" ~: runParser int (T.pack "123a") ~?= success 123 (T.pack "a")
            , "parse negative int -123 with rest" ~: runParser int (T.pack "-123a") ~?= success (-123) (T.pack "a")
            , "fail on non-digit start 'a123'" ~: runParser int (T.pack "a123") ~?= expectError "not predicate at a" (T.pack "123")
            , "fail on '-' then non-digit '-a123'" ~: runParser int (T.pack "-a123") ~?= expectError "not predicate at -" (T.pack "a123")
            , "fail on just '-'" ~: runParser int (T.pack "-") ~?= expectError "not predicate at -" (T.pack "")
            , "fail on empty for int" ~: runParser int (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            ]

tokenTest :: Test
tokenTest =
    "token (Text)"
        ~: TestList
            [ "tokenize digit '1'" ~: runParser (token digit) (T.pack "1") ~?= success '1' (T.pack "")
            , "tokenize digit ' 1 '" ~: runParser (token digit) (T.pack " 1 ") ~?= success '1' (T.pack "")
            , "tokenize digit '  1'" ~: runParser (token digit) (T.pack "  1") ~?= success '1' (T.pack "")
            , "tokenize digit '1  '" ~: runParser (token digit) (T.pack "1  ") ~?= success '1' (T.pack "")
            , "tokenize digit '  123'" ~: runParser (token digit) (T.pack "  123") ~?= success '1' (T.pack "23")
            , "tokenize digit '1   abc'" ~: runParser (token digit) (T.pack "1   abc") ~?= success '1' (T.pack "abc") -- token consumes "   " after '1'
            , "tokenize string \"word\" with spaces" ~: runParser (token (string "word")) (T.pack "  word  ") ~?= success "word" (T.pack "")
            , "tokenize nat with spaces" ~: runParser (token nat) (T.pack "  123  ") ~?= success (123 :: Int) (T.pack "")
            , "fail token nat on '  -123  '" ~: runParser (token nat) (T.pack "  -123  ") ~?= expectError "not predicate at -" (T.pack "123  ")
            , "fail token digit on ' abc '" ~: runParser (token digit) (T.pack " abc ") ~?= expectError "not predicate at a" (T.pack "bc ")
            , "token on empty input with parser that succeeds on empty (like string \"\")" ~: runParser (token (string "")) (T.pack "") ~?= success "" (T.pack "")
            , "token on empty input with parser that fails on empty (like digit)" ~: runParser (token digit) (T.pack "") ~?= expectRunParserEmpty (T.pack "")
            , "token on spaces only with parser that fails on empty (like digit)" ~: runParser (token digit) (T.pack "   ") ~?= expectRunParserEmpty (T.pack "")
            ]
