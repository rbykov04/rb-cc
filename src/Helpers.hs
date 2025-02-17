module Helpers where

import Tokenize
import Error
import Parse2 as RawCStage

import System.IO
import System.Environment
import Text.Printf (printf)

import Data.List.Split

errAdapter :: Either (Int, String) a -> Either Error a
errAdapter (Left (loc, text)) = Left $ ErrorLoc loc text
errAdapter (Right a) = Right a

prepareTestFile :: String -> Either Error (String, String)
prepareTestFile file = do
  let (_ : c_prog : _ : stage1 :_ ) = splitOn "\n//==\n" file
  return (c_prog, stage1)

printTextErr :: [String] -> IO (Int)
printTextErr [] = return 1
printTextErr (a : as) = do
  hPutStrLn stderr a
  printTextErr as

printlnProgram :: [String] -> IO ()
printlnProgram [] = return ()
printlnProgram (s:ss) = do
  printf "%s\n" s
  printlnProgram ss



bind3args out in3 = let
    e = (.) out
    g = (.) e
    f = (.) g
    in f in3

ppStage1 :: String -> Either Error [String]
ppStage1 c_text = do
  toks <- (errAdapter . tokenize_) c_text
  prog <- (parse2 . convert_keywords) toks
  return $ RawCStage.printProgram prog

printDiff :: String -> String -> IO ()
printDiff a b = do
  printf "===== result =============\n"
  printf "%s\n" a
  printf "===== etalon =============\n"
  printf "%s\n" b
  printf "==========================\n"
