module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit
import CInterpreter
import Tokenize
import Stage1
import MetaIR
import Error
import Helpers
import Data.List.Split
import System.IO
import Data.List

foo :: String -> Either Error (String, String, String)
foo file = do
    (c, c1) <- prepareTestFile file
    text <- ppStage1 c
    let res = intercalate "\n" text
    return (c, c1, res)

prettyPrintMetaIR :: String -> Either Error (String, String, String)
prettyPrintMetaIR file = do
    (c, c1) <- prepareTestFile file
    text <- ppStage3 c
    let res = intercalate "\n" text
    return (c, c1, res)

compareTwoIR :: String -> Either Error (String, String, String)
compareTwoIR file = do
    sections  <- parseTestFile file
    let (TestSection c1 : TestSection c2 : _) = sections
    let a = snd c1
    let b = snd c2
    toks <- (errAdapter . tokenize_) a
    prog <- Stage1.parseIR toks
    let meta1 = des prog

    toks2 <- (errAdapter . tokenize_) b
    prog2 <- Stage1.parseIR toks2
    let meta2 = des prog2
    assertEquality meta1 meta2
    return (a, "They are Equal", "They are Equal")

my_test func filename = do
    file <- readFile filename
    case func file of
        Left e -> do
            let report = intercalate "\n" $ printError "" e
            assertFailure (report ++ "\n filename: \n" ++ filename)
        Right (i, etalon, res)  -> do
            if res == etalon
            then return ()
            else do
                printDiff3 i res etalon
                assertFailure ""

test1 = TestCase (my_test prettyPrintMetaIR      "test/Parse2/prettyPrintMetaIR.test")
test2 = TestCase (my_test foo                    "test/Parse2/vars.test")
test3 = TestCase (my_test compareTwoIR           "test/Parse2/compareTwoIR.test")
current = TestCase (my_test foo         "test/Parse2/func.test")

tests = TestList
    [
      test1
    , test2
    , test3
    , current
    ]

main :: IO ()
main = runTestTTAndExit tests
