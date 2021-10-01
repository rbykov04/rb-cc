module Main where
import System.Environment
import System.IO
import Text.Printf

getInt :: String -> Int
getInt s = read s

main :: IO (Int)
main = do
  args <- getArgs
  if length args < 1 then do
    hPutStrLn stderr "rb-cc: invalid number of arguments"
    return 1
  else do
    let ret = (show. getInt .head) args
    printf "  .globl main\n"
    printf "main:\n"
    printf "  mov $%s, %%rax\n" ret
    printf "  ret\n"
    return 0
