module Main where


import System.Environment
import System.IO
import Codegen
import Tokenize
import Parse
import Semantic
import Error
import AST
import Text.Printf
import Data.List
import Text.Pretty.Simple
import System.Exit (exitFailure)
import Data.Text.Lazy (unpack)
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap


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

data Mode = Compile | Dump deriving (Eq, Show)
data Args = Args
  {
    opt_o      :: String,
    input_path :: String,
    mode       :: Mode
  }
  deriving (Eq, Show)


parseArgs :: [String] -> Either (Int, String) Args
parseArgs []   = Left (-1, "rb-cc: invalid number of arguments")
parseArgs (opt : v : _) =
  if opt == "-o"
  then Right (Args v "-" Compile)
  else if opt == "-d"
    then Right (Args v "-"  Dump)
    else Left (-1, "rb-cc: unknown opt" ++ opt)
parseArgs args = Left (-1, "rb-cc: unknown opts " ++ show args)


printTextErr :: [String] -> IO (Int)
printTextErr [] = return 1
printTextErr (a : as) = do
  hPutStrLn stderr a
  printTextErr as

bind3args out in3 = let
    e = (.) out
    g = (.) e
    f = (.) g
    in f in3

printError' :: String -> Error -> IO (Int)
printError' input err = do
  printTextErr (printError input err)
  exitFailure

errorAt' = bind3args printTextErr errorAt


debugPrint :: String -> Error -> Maybe [Obj] -> Maybe [Obj] -> Mode -> IO (Int)
debugPrint file err globals checkedGlobals mode = do
  let dump = case mode of
       Compile -> ""
       Dump ->  "untyped Tree: \n "
          ++        addDump globals
          ++    "\ntyped Tree (before codegen): \n"
          ++        addDump checkedGlobals
          ++ "\n"

  printTextErr (printError file err)
--  printf "%s" dump
  exitFailure
    where
      addDump t = case t of
          Nothing -> "NONE"
          Just g -> unpack (pShow g)

assembleGlobals :: [Obj] -> IntMap Obj -> [Obj]
assembleGlobals globals storage = map restoreFunc globals
  where
    restoreFunc g = case IntMap.lookup (objKey g) storage of
      Just actualObj -> actualObj
      Nothing        -> g


main :: IO (Int)
main = do
  args <- getArgs
  case parseArgs args of
    Left (err, text) -> do
      hPutStrLn stderr text
      hPrint stderr err
      return err
    Right rbArgs -> do
      let path = input_path rbArgs
      file <- readCFile path
      let res = tokenize_ file
      case res of
        Left (loc, text) -> do
          errorAt' file loc text
        Right toks -> do
          let parse_res = (parse . convert_keywords) toks
          case parse_res of
            Left err -> printError' file err
            Right (globals, toks, storage) ->
              case typecheck globals storage of
                Left err -> do
                      let dump = assembleGlobals globals storage
                      debugPrint file err (Just dump) Nothing (mode rbArgs)
                Right (checkedGlobals, checkedStorage) -> do
                  if mode rbArgs == Compile
                    then
                    case codegen checkedGlobals checkedStorage of
                      Right prog -> do
                        let filename = opt_o rbArgs
                        if  filename == ""
                        then do printProgram prog
                        else do writeFile filename (intercalate "" prog)

                        return 0
                      Left e -> do
                        printError' file e
                    else do
                      --dump
                          let symTable = assembleGlobals checkedGlobals checkedStorage
                          let dump = unpack (pShowNoColor symTable)
                          let filename = opt_o rbArgs
                          if  filename == ""
                          then do printf "%s\n" dump
                          else do writeFile filename dump
                          return 0
