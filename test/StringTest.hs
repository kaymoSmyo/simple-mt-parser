module StringTest (stringToolTest) where

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

-- System.Exit and parts of Test.HUnit (Counts, runTestTT) are removed
-- as Main.hs will handle the overall test execution.
import Test.HUnit (
    Test (TestList),
    (~:),
    (~?=),
 )

stringToolTest :: Test -- Renamed from allTests to be more specific
stringToolTest =
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
success :: (Show a, Eq a) => a -> String -> (Either ParseError a, String)
success val rest = (Right val, rest)

-- Specific error message and remaining string at the point of error
expectError :: (Show a, Eq a) => String -> String -> (Either ParseError a, String)
expectError msg expectedRest = (Left (Err msg), expectedRest)

-- Default error from runParser when a parser yields no result (returns [])
expectRunParserEmpty :: (Show a, Eq a) => String -> (Either ParseError a, String)
expectRunParserEmpty originalInput = (Left (Err "Empty Input"), originalInput)

-- Tests for each parser from MTParser.Tools

digitTest :: Test
digitTest =
    "digit"
        ~: TestList
            [ "parse '1'" ~: runParser digit "1" ~?= success '1' ""
            , "parse '7' with rest" ~: runParser digit "7abc" ~?= success '7' "abc"
            , "fail on space then '1'" ~: runParser digit " 1" ~?= expectError "not predicate at  " "1"
            , "fail on 'a'" ~: runParser digit "a" ~?= expectError "not predicate at a" ""
            , "fail on 'a' with rest" ~: runParser digit "abc" ~?= expectError "not predicate at a" "bc"
            , "fail on empty for digit" ~: runParser digit "" ~?= expectRunParserEmpty ""
            ]

lowerTest :: Test
lowerTest =
    "lower"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser lower "a" ~?= success 'a' ""
            , "parse lowercase 'z' with rest" ~: runParser lower "zXY" ~?= success 'z' "XY"
            , "fail on uppercase 'A'" ~: runParser lower "A" ~?= expectError "not predicate at A" ""
            , "fail on uppercase 'B' with rest" ~: runParser lower "BC" ~?= expectError "not predicate at B" "C"
            , "fail on digit '1'" ~: runParser lower "1" ~?= expectError "not predicate at 1" ""
            , "fail on empty" ~: runParser lower "" ~?= expectRunParserEmpty ""
            ]

upperTest :: Test
upperTest =
    "upper"
        ~: TestList
            [ "parse uppercase 'A'" ~: runParser upper "A" ~?= success 'A' ""
            , "parse uppercase 'Z' with rest" ~: runParser upper "Zxy" ~?= success 'Z' "xy"
            , "fail on lowercase 'a'" ~: runParser upper "a" ~?= expectError "not predicate at a" ""
            , "fail on lowercase 'b' with rest" ~: runParser upper "bc" ~?= expectError "not predicate at b" "c"
            , "fail on digit '1'" ~: runParser upper "1" ~?= expectError "not predicate at 1" ""
            , "fail on empty" ~: runParser upper "" ~?= expectRunParserEmpty ""
            ]

letterTest :: Test
letterTest =
    "letter"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser letter "a" ~?= success 'a' ""
            , "parse uppercase 'A'" ~: runParser letter "A" ~?= success 'A' ""
            , "parse letter 'x' with rest" ~: runParser letter "x123" ~?= success 'x' "123"
            , "fail on digit '1'" ~: runParser letter "1" ~?= expectError "not predicate at 1" ""
            , "fail on symbol '*'" ~: runParser letter "*" ~?= expectError "not predicate at *" ""
            , "fail on empty" ~: runParser letter "" ~?= expectRunParserEmpty ""
            ]

alphanumTest :: Test
alphanumTest =
    "alphanum"
        ~: TestList
            [ "parse lowercase 'a'" ~: runParser alphanum "a" ~?= success 'a' ""
            , "parse uppercase 'A'" ~: runParser alphanum "A" ~?= success 'A' ""
            , "parse digit '7'" ~: runParser alphanum "7" ~?= success '7' ""
            , "parse alphanum 'Z' with rest" ~: runParser alphanum "Z==" ~?= success 'Z' "=="
            , "fail on symbol '-'" ~: runParser alphanum "-" ~?= expectError "not predicate at -" ""
            , "fail on empty" ~: runParser alphanum "" ~?= expectRunParserEmpty ""
            ]

charTest :: Test
charTest =
    "char"
        ~: TestList
            [ "parse matching char 'x'" ~: runParser (char 'x') "x" ~?= success 'x' ""
            , "parse matching char 'y' with rest" ~: runParser (char 'y') "yz" ~?= success 'y' "z"
            , "fail on non-matching char 'x' for 'y'" ~: runParser (char 'x') "y" ~?= expectError "not predicate at y" ""
            , "fail on non-matching char 'a' for 'b' with rest" ~: runParser (char 'a') "bc" ~?= expectError "not predicate at b" "c"
            , "fail on empty for char 'x'" ~: runParser (char 'x') "" ~?= expectRunParserEmpty ""
            ]

stringTest :: Test
stringTest =
    "string"
        ~: TestList
            [ "parse matching string \"abc\"" ~: runParser (string "abc") "abc" ~?= success "abc" ""
            , "parse matching string \"hello\" with rest" ~: runParser (string "hello") "helloworld" ~?= success "hello" "world"
            , "fail on partial match \"ab\" for \"abc\"" ~: runParser (string "abc") "ab" ~?= expectRunParserEmpty ""
            , "fail on mismatch \"axc\" for \"abc\"" ~: runParser (string "abc") "axc" ~?= expectError "not predicate at x" "c"
            , "parse empty string" ~: runParser (string "") "abc" ~?= success "" "abc"
            , "parse empty string on empty input" ~: runParser (string "") "" ~?= success "" ""
            , "fail on non-empty string \"a\" on empty input" ~: runParser (string "a") "" ~?= expectRunParserEmpty ""
            ]

identTest :: Test
identTest =
    "ident"
        ~: TestList
            [ "parse single lower 'a'" ~: runParser ident "a" ~?= success "a" ""
            , "parse lower then alphanum 'a1'" ~: runParser ident "a1" ~?= success "a1" ""
            , "parse multiple lower then multiple alphanum 'abc123xyz'" ~: runParser ident "abc123xyz" ~?= success "abc123xyz" ""
            , "parse only 'lower' case" ~: runParser ident "lower" ~?= success "lower" ""
            , "stop at non-alphanum 'ident-'" ~: runParser ident "ident-" ~?= success "ident" "-"
            , "parse 'aBc' as 'aBc'" ~: runParser ident "aBc" ~?= success "aBc" ""
            , "parse 'camelCase'" ~: runParser ident "camelCase" ~?= success "camelCase" ""
            , "fail on starting with digit '1a'" ~: runParser ident "1a" ~?= expectError "not predicate at 1" "a"
            , "fail on starting with uppercase 'Ident'" ~: runParser ident "Ident" ~?= expectError "not predicate at I" "dent"
            , "fail on starting with symbol '_ident'" ~: runParser ident "_ident" ~?= expectError "not predicate at _" "ident"
            , "fail on empty for ident" ~: runParser ident "" ~?= expectRunParserEmpty ""
            ]

natTest :: Test
natTest =
    "nat"
        ~: TestList
            [ "parse positive number 123" ~: runParser nat "123" ~?= success 123 ""
            , "parse zero 0" ~: runParser nat "0" ~?= success 0 ""
            , "parse number 123 with trailing 'a'" ~: runParser nat "123a" ~?= success 123 "a"
            , "fail on non-digit start 'a123'" ~: runParser nat "a123" ~?= expectError "not predicate at a" "123"
            , "fail on empty for nat" ~: runParser nat "" ~?= expectRunParserEmpty ""
            , "parse single digit 5" ~: runParser nat "5" ~?= success 5 ""
            ]

spaceTest :: Test
spaceTest =
    "space"
        ~: TestList
            [ "consume multiple spaces '   '" ~: runParser space "   " ~?= success () ""
            , "consume spaces before text '   abc'" ~: runParser space "   abc" ~?= success () "abc"
            , "consume no spaces if none present 'abc'" ~: runParser space "abc" ~?= success () "abc"
            , "consume no spaces on empty input ''" ~: runParser space "" ~?= success () ""
            ]

intTest :: Test
intTest =
    "int"
        ~: TestList
            [ "parse positive int 123" ~: runParser int "123" ~?= success 123 ""
            , "parse negative int -123" ~: runParser int "-123" ~?= success (-123) ""
            , "parse zero 0" ~: runParser int "0" ~?= success 0 ""
            , "parse negative zero -0" ~: runParser int "-0" ~?= success 0 "" -- -0 is 0
            , "parse positive int 123 with rest" ~: runParser int "123a" ~?= success 123 "a"
            , "parse negative int -123 with rest" ~: runParser int "-123a" ~?= success (-123) "a"
            , "fail on non-digit start 'a123'" ~: runParser int "a123" ~?= expectError "not predicate at a" "123"
            , "fail on '-' then non-digit '-a123'" ~: runParser int "-a123" ~?= expectError "not predicate at -" "a123"
            , "fail on just '-'" ~: runParser int "-" ~?= expectError "not predicate at -" ""
            , "fail on empty for int" ~: runParser int "" ~?= expectRunParserEmpty ""
            ]

tokenTest :: Test
tokenTest =
    "token"
        ~: TestList
            [ "tokenize digit '1'" ~: runParser (token digit) "1" ~?= success '1' ""
            , "tokenize digit ' 1 '" ~: runParser (token digit) " 1 " ~?= success '1' ""
            , "tokenize digit '  1'" ~: runParser (token digit) "  1" ~?= success '1' ""
            , "tokenize digit '1  '" ~: runParser (token digit) "1  " ~?= success '1' ""
            , "tokenize digit '  123'" ~: runParser (token digit) "  123" ~?= success '1' "23" -- token consumes leading space, digit consumes '1', token consumes no trailing space before "23"
            , "tokenize digit '1   abc'" ~: runParser (token digit) "1   abc" ~?= success '1' "abc" -- token no lead, digit '1', token consumes "   ", rest "abc"
            , "tokenize string \"word\" with spaces" ~: runParser (token (string "word")) "  word  " ~?= success "word" ""
            , "tokenize nat with spaces" ~: runParser (token nat) "  123  " ~?= success 123 ""
            , "fail token nat on '  -123  '" ~: runParser (token nat) "  -123  " ~?= expectError "not predicate at -" "123  "
            , "fail token digit on ' abc '" ~: runParser (token digit) " abc " ~?= expectError "not predicate at a" "bc "
            , "token on empty input with parser that succeeds on empty (like string \"\")" ~: runParser (token (string "")) "" ~?= success "" ""
            , "token on empty input with parser that fails on empty (like digit)" ~: runParser (token digit) "" ~?= expectRunParserEmpty ""
            , "token on spaces only with parser that fails on empty (like digit)" ~: runParser (token digit) "   " ~?= expectRunParserEmpty ""
            ]
