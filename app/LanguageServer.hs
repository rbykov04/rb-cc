module Main where


import System.Environment
import System.IO
import RBCC
import Codegen
import Tokenize
import Parse
import Error
import Text.Printf
import Data.List

--import qualified MyLib (someFunc)


printProgram :: [String] -> IO ()
printProgram [] = return ()
printProgram (s:ss) = do
  printf "%s" s
  printProgram ss

printlnProgram :: [String] -> IO ()
printlnProgram [] = return ()
printlnProgram (s:ss) = do
  printf "%s\n" s
  printlnProgram ss




printTokens :: [Token] -> [String]
printTokens [] = []
printTokens (s:ss) = [(show s)] ++ printTokens ss

main :: IO (Int)
main = do
  hPutStrLn stdout "============================="
  file <- getContents
  hPutStrLn stdout file
  hPutStrLn stdout "============================="
  let res = tokenize_ file
  case res of
    Left (loc, text) -> do
      errorAt file loc text
    Right toks -> do
      let t = printTokens toks
      printlnProgram t
      hPutStrLn stdout "============================="

      let parse_res = (parse . convert_keywords) toks
      case parse_res of
        Left err -> printError file err
        Right (globals, _, storage) ->
          case codegen globals storage of
          Right prog -> do
            printProgram prog
            hPutStrLn stdout "============================="
            return 0
          Left e -> do
            printError file e
