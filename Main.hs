module Main where
import System.Environment
import System.IO
import Codegen
import Tokenize
import Parse
import Text.Printf


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


main :: IO (Int)
main = do
  args <- getArgs
  if length args < 1 then do
    hPutStrLn stderr ("rb-cc: invalid number of arguments " ++ show args)
    return (-1)
  else do
    let path = head args
    file <- readCFile path
    let res = tokenize_ file
    case res of
      Left (loc, text) -> do
        error_at file loc text
      Right toks -> do
        let parse_res = (parse . convert_keywords) toks
        case parse_res of
          Left err -> printError file err
          Right (globals, _, storage) ->
            case codegen globals storage of
            Right prog -> do
              printProgram prog
              return 0
            Left e -> do
              printError file e
