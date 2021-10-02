module Main where
import System.Environment
import System.IO
import Text.Printf
import Data.Char


getInt :: String -> Int
getInt s = read s

strtol :: Int -> String -> (Int, String, Int)
strtol = helper 0 where
  helper res c [] = (res,[], c)
  helper res 0 xs = (res, xs, 0)
  helper res count (x:xs)
    | isDigit x = helper (res* 10 + digitToInt x) (count-1) xs
    | otherwise = (res, x:xs, count)


data TokenKind =
  EOF
  | Punct String
  | Num Int
  deriving (Eq, Show)

data Token = Token
  {
    tokenKind :: TokenKind,
    tokenLen :: Int,
    tokenLoc :: Int
  }
  deriving (Eq, Show)

error_at :: String -> Int -> String -> IO (Int)
error_at current_input loc text = do
  hPutStrLn stderr $ current_input
  hPutStrLn stderr $ replicate loc ' ' ++ "^ " ++ text
  return 1


error_tok :: String -> Token -> String -> IO (Int)
error_tok input tok = error_at input (tokenLoc tok)

tokenize :: Int -> String -> [Either (Int, String) Token]
tokenize c [] = [Right$ Token EOF 0 c]
tokenize c (p:ps)
  | p == ' '  = tokenize (c+1) ps
  | p == '+'  = (Right $ Token (Punct "+") 1 c) : tokenize (c+1) ps
  | p == '-'  = (Right $ Token (Punct "-") 1 c) : tokenize (c+1) ps
  | isDigit p = (Right $ Token (Num number) len c) : tokenize (c+len) pss
  | otherwise = [Left (c, "invalid token")]
  where
    (number, pss, x) = strtol 10 (p:ps)
    len = 10 - x

get_number :: String -> Token -> IO (Maybe Int)
get_number input (Token (Num v) _ _) = return (Just v)
get_number input tok = do
  error_tok input tok "expected a number"
  return Nothing


compile :: String -> [Token] -> IO (Int)
compile input [] = return 0
compile input ((Token (Punct x) _ _): ts)
  | x == "+" = do
      let tok = head ts
      res <- get_number input tok
      case res of
        Just v -> do
          printf "  add $%d, %%rax\n" v
          compile input $ tail ts
        Nothing -> return 1
  | x == "-" = do
      let tok = head ts
      res <- get_number input tok
      case res of
        Just v -> do
          printf "  sub $%d, %%rax\n" v
          compile input $ tail ts
        Nothing -> return 1
  | otherwise = do
    hPutStrLn stderr $ "Unexpected punct" ++ x
    return 1

compile _ ((Token (EOF) _ _): ts) = return 0

compile _ t@((Token (Num v) _ _): ts) = do
  hPutStrLn stderr $ "Unexpected token" ++ show t
  return 0

main :: IO (Int)
main = do
  args <- getArgs
  if length args < 1 then do
    hPutStrLn stderr "rb-cc: invalid number of arguments"
    return 1
  else do
    let p = head args
    let res = sequence $ tokenize 0 p
    printf "  .globl main\n"
    printf "main:\n"
    case res of
      Left (loc, text) -> do
        error_at p loc text
      Right toks -> do
        let (Num a) = tokenKind $ head toks
        printf "  mov $%d, %%rax\n" a
        ret <- compile p $tail toks
        if ret == 0
          then do
            printf "  ret\n"
            return 0
          else return ret
