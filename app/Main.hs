module Main where


import System.Environment
import System.IO
import Codegen
import Tokenize
import Parse
import Semantic
import Error
import AST
import Driver
import Text.Printf
import Data.List
import Text.Pretty.Simple
import System.Exit (exitFailure)
import Data.Text.Lazy (unpack)
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap


printProgram :: [String] -> IO ()
printProgram [] = return ()
printProgram (s:ss) = do
  printf "%s" s
  printProgram ss

readCFile :: String -> IO (String)
-- By convention, read from stdin if a given filename is "-".
readCFile ('-': _) = do
  text <- getContents
  return text
readCFile path = do
    hPutStrLn stderr ("rb-cc: can't read from" ++ show (path))
    return ("")

data Mode = Compile | Dump deriving (Eq, Show)
data Args = Args
  {
    opt_o      :: String,
    input_path :: String,
    mode       :: Mode
  }
  deriving (Eq, Show)


parseArgs :: [String] -> Either (Int, String) Args
parseArgs []   = Left (-1, "rb-cc: invalid number of arguments")
parseArgs (opt : v : _) =
  if opt == "-o"
  then Right (Args v "-" Compile)
  else if opt == "-d"
    then Right (Args v "-"  Dump)
    else Left (-1, "rb-cc: unknown opt" ++ opt)
parseArgs args = Left (-1, "rb-cc: unknown opts " ++ show args)


printTextErr :: [String] -> IO (Int)
printTextErr [] = return 1
printTextErr (a : as) = do
  hPutStrLn stderr a
  printTextErr as

printError' :: String -> Error -> IO (Int)
printError' input err = do
  printTextErr (printError input err)
  exitFailure


main :: IO (Int)
main = do
  args <- getArgs
  case parseArgs args of
    Left (err, text) -> do
      hPutStrLn stderr text
      hPrint stderr err
      return err
    Right rbArgs -> do
      let path = input_path rbArgs
      let filename = opt_o rbArgs
      file <- readCFile path
      case mode rbArgs of
        Compile -> case compile file of
          Right prog -> do
            if  filename == ""
            then do printProgram prog
            else do writeFile filename (intercalate "" prog)
            return 0
          Left e -> do
            printError' file e
            return (-1)
        Dump -> case debugMode file of
          Right dump -> do
            if  filename == ""
            then do printf "%s\n" dump
            else do writeFile filename dump
            return 0
          Left e -> do
            printError' file e
            return (-1)
