module Main where
import System.Environment
import System.IO
import Codegen
import Tokenize
import Parse

main :: IO (Int)
main = do
  args <- getArgs
  if length args < 1 then do
    hPutStrLn stderr "rb-cc: invalid number of arguments"
    return 1
  else do
    let p = head args
    let res = sequence $ tokenize 0 p
    case res of
      Left (loc, text) -> do
        error_at p loc text
      Right toks -> do
        let parse_res = parse toks
        case parse_res of
          Left err -> printError p err
          Right (func, _) -> codegen func
