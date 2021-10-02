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

tokenize :: Int -> String -> [Either String Token]
tokenize c [] = [Right$ Token EOF 0 c]
tokenize c (p:ps)
  | p == ' '  = tokenize (c+1) ps
  | p == '+'  = (Right $ Token (Punct "+") 1 c) : tokenize (c+1) ps
  | p == '-'  = (Right $ Token (Punct "-") 1 c) : tokenize (c+1) ps
  | isDigit p = (Right $ Token (Num number) len c) : tokenize (c+len) pss
  | otherwise = [Left $ "pos " ++ show c ++ ": Unknown token: " ++ p:ps]
  where
    (number, pss, x) = strtol 10 (p:ps)
    len = 10 - x

compile :: [Token] -> IO (Int)
compile [] = return 0
compile ((Token (Punct x) _ _): ts)
  | x == "+" = do
      case head ts of
        (Token (Num v) _ _) -> do
          printf "  add $%d, %%rax\n" v
          compile $ tail ts
        t@(Token _ len c) -> do
          hPutStrLn stderr $ "Unexpected token" ++ show t
          return 1
  | x == "-" = do
      case head ts of
        (Token (Num v) _ _) -> do
          printf "  sub $%d, %%rax\n" v
          compile $ tail ts
        t@(Token _ len c) -> do
          hPutStrLn stderr $ "Unexpected token" ++ show t
          return 1
  | otherwise = do
    hPutStrLn stderr $ "Unexpected punct" ++ x
    return 1

compile ((Token (EOF) _ _): ts) = return 0

compile t@((Token (Num v) _ _): ts) = do
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
      Left e -> do
        hPutStrLn stderr e
        return 1
      Right toks -> do
        let (Num a) = tokenKind $ head toks
        printf "  mov $%d, %%rax\n" a
        ret <- compile $tail toks
        if ret == 0
          then do
            printf "  ret\n"
            return 0
          else return ret
