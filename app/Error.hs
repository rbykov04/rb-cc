module Error where
import RBCC
import System.IO

error_at :: String -> Int -> String -> IO (Int)
error_at current_input loc text = do
  hPutStrLn stderr $ current_input
  hPutStrLn stderr $ replicate loc ' ' ++ "^ " ++ text
  return 1

error_tok :: String -> Token -> String -> IO (Int)
error_tok input tok text = error_at input (tokenLoc tok) $ text ++ "\n"  ++ show tok



printError :: String -> Error -> IO (Int)
printError input (ErrorToken t text) = do
  error_tok input t text
printError input (ErrorText text) = do
  hPutStrLn stderr $ text
  return 1
