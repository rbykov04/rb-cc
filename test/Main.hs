module Main (main) where

import Test.HUnit
import qualified System.Exit as Exit
import CInterpreter
import Tokenize
import Parse2 as RawCStage
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



my_test func filename = do
    file <- readFile filename
    case func file of
        Left e -> do
            let report = intercalate "\n" $ printError "" e
            assertFailure report
        Right (i, etalon, res)  -> do
            if res == etalon
            then return ()
            else do
                printDiff3 i res etalon
                assertFailure ""

test2 = TestCase (my_test foo                    "test/Parse2/vars.test")
test1 = TestCase (my_test prettyPrintMetaIR      "test/Parse2/current.test")

tests = TestList [test1, test2]

main :: IO ()
main = runTestTTAndExit tests
