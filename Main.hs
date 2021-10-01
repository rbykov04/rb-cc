module Main where
import System.Environment
import System.IO
import Text.Printf
import Data.Char

getInt :: String -> Int
getInt s = read s

strtol :: Int -> String -> (Int, String)
strtol = helper 0 where
  helper res _ [] = (res,[])
  helper res 0 xs = (res, xs)
  helper res count (x:xs)
    | isDigit x = helper (res* 10 + digitToInt x) (count-1) xs
    | otherwise = (res, x:xs)

compile :: String -> IO (Int)
compile [] = return 0
compile (x:xs)
  | x == '+' = do
      let (i, xss) = strtol 10 xs
      printf "  add $%d, %%rax\n" i
      compile xss

  | x == '-' = do
      let (i, xss) = strtol 10 xs
      printf "  sub $%d, %%rax\n" i
      compile xss


  | otherwise = do
        hPutStrLn stderr $ "rb-cc: unexpected character: " ++ [x]
        return 1


main :: IO (Int)
main = do
  args <- getArgs
  if length args < 1 then do
    hPutStrLn stderr "rb-cc: invalid number of arguments"
    return 1
  else do
    printf "  .globl main\n"
    printf "main:\n"

    let p = head args
    let (a, ps) = strtol 10 p
    printf "  mov $%d, %%rax\n" a
    ret <- compile ps
    if ret == 0
      then do
        printf "  ret\n"
        return 0
      else return ret
