module Main where
import System.Environment
import System.IO
import Codegen
import Tokenize
import Parse
import Text.Printf
import Data.List


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

data Args = Args
  {
    opt_o      :: String,
    input_path :: String
  }
  deriving (Eq, Show)


parseArgs :: [String] -> Either (Int, String) Args
parseArgs []   = Left (-1, "rb-cc: invalid number of arguments")
parseArgs (opt : v : _) =
  if opt == "-o"
  then Right (Args v "-")
  else Left (-1, "rb-cc: unknown opt" ++ opt)
parseArgs args = Left (-1, "rb-cc: unknown opts " ++ show args)

main :: IO (Int)
main = do
  args <- getArgs
  case parseArgs args of
    Left (err, text) -> do
      hPutStrLn stderr text
      return err
    Right rbArgs -> do
      let path = input_path rbArgs
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
                let filename = opt_o rbArgs
                if  filename == ""
                then do printProgram prog
                else do writeFile filename (intercalate "" prog)

                return 0
              Left e -> do
                printError file e
