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

foo :: String -> Either Error (String, String)
foo file = do
    (c, c1) <- prepareTestFile file
    text <- ppStage1 c
    let res = intercalate "\n" text
    return (c1, res)

my_test filename = do
    file <- readFile filename
    case foo file of
        Left e -> do
            let report = intercalate "\n" $ printError "" e
            assertFailure report
        Right (etalon, res)  -> do
            if res == etalon
            then return ()
            else do
                printDiff res etalon
                assertFailure ""

test1 = TestCase (my_test "test/Parse2/current.test")
test2 = TestCase (my_test "test/Parse2/vars.test")

tests = TestList [test1, test2]

main :: IO ()
main = runTestTTAndExit tests
