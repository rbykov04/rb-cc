module Main (main) where
import Test.HUnit
import qualified System.Exit as Exit
import CInterpreter
import Tokenize

import Parse2 as RawCStage
import Error
import Data.List.Split
import System.IO

prepareTestFile :: String -> Either Error (String, String)
prepareTestFile file = do
  let (_ : c_prog : _ : stage1 :_ ) = splitOn "//==" file
  return (c_prog, stage1)


errAdapter :: Either (Int, String) a -> Either Error a
errAdapter (Left (loc, text)) = Left $ ErrorLoc loc text
errAdapter (Right a) = Right a

printTextErr :: [String] -> IO (Int)
printTextErr [] = return 1
printTextErr (a : as) = do
  hPutStrLn stderr a
  printTextErr as

bind3args out in3 = let
    e = (.) out
    g = (.) e
    f = (.) g
    in f in3

printError' :: String -> Error -> IO (Int)
printError' input err = do
  printTextErr (printError input err)

errorAt' = bind3args printTextErr errorAt
errorTok' = bind3args printTextErr errorTok


foo file = do
        (c, c1) <- prepareTestFile file

        toks    <- (errAdapter . tokenize_) c
        prog    <- (parse2 . convert_keywords) toks

        toks2   <- (errAdapter . tokenize_) c1
        prog2   <- RawCStage.parseIR toks2
        return ((c, prog), (c1, prog2))


my_test = do
        file <- readFile "test/Parse2/smoke.test"
        case foo file of
                Left e -> do
                        printError' "" e
                        assertFailure "we can't parse"
                Right ((c1, ir1) , (c2, ir2))  -> do
                  let a = des ir1
                  let b = des ir2

                  case assertEq a b of
                    Left (ctx1, ctx2) -> do
                        assertFailure (c1
                                       ++ "\n======== are not equal ====\n"
                                       ++ c2
                                       ++ show (a)
                                       ++ "\n======== are not equal ====\n"
                                       ++ show (b)
                                       ++ "\n-----------------\n"
                                       ++ show (ctx1)
                                       ++ "\n======== are not equal ====\n"
                                       ++ show (ctx2))
                    Right () -> do
                      assertBool "" True

tests = TestCase my_test


main :: IO ()
main = runTestTTAndExit tests


    --if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
