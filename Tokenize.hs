module Tokenize where
import System.Environment
import System.IO
import Text.Printf
import Data.Char
import RBCC

strtol :: Int -> String -> (Int, String, Int)
strtol = helper 0 where
  helper res c [] = (res,[], c)
  helper res 0 xs = (res, xs, 0)
  helper res count (x:xs)
    | isDigit x = helper (res* 10 + digitToInt x) (count-1) xs
    | otherwise = (res, x:xs, count)

error_at :: String -> Int -> String -> IO (Int)
error_at current_input loc text = do
  hPutStrLn stderr $ current_input
  hPutStrLn stderr $ replicate loc ' ' ++ "^ " ++ text
  return 1

error_tok :: String -> Token -> String -> IO (Int)
error_tok input tok text = error_at input (tokenLoc tok) $ text ++ "\n"  ++ show tok

tokenize :: Int -> String -> [Either (Int, String) Token]
tokenize c [] = [Right$ Token EOF 0 c]
tokenize c (p:ps)
  | not (ps == []) &&
      (
          (p == '>' && head ps == '=')
       || (p == '<' && head ps == '=')
       || (p == '!' && head ps == '=')
       || (p == '=' && head ps == '=')

       )
        =(Right $ Token (Punct (p:(head ps):[])) 2 c) : tokenize (c+2) (tail ps)

  | isSeparator p = tokenize (c+1) ps
  | isPunctuation p
    || isSymbol p
    || p == '>' || p == '<'
      =(Right $ Token (Punct [p]) 1 c) : tokenize (c+1) ps

  | isDigit p = (Right $ Token (Num number) len c) : tokenize (c+len) pss
  | isAlpha p = (Right $ Token (Ident [p]) 1 c)    : tokenize (c+1) ps
  | otherwise = [Left (c, "invalid token")]
  where
    (number, pss, x) = strtol 10 (p:ps)
    len = 10 - x

get_number :: String -> Token -> IO (Maybe Int)
get_number input (Token (Num v) _ _) = return (Just v)
get_number input tok = do
  error_tok input tok "expected a number"
  return Nothing

printError :: String -> Error -> IO (Int)
printError input (ErrorToken t text) = do
  error_tok input t text


