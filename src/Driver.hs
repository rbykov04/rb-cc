module Driver where


import Codegen
import Tokenize
import Parse
import Semantic
import Error
import AST
import Semantic (typecheck)
import Scopechecker (scopecheck)
import Text.Printf
import Data.List
import Text.Pretty.Simple
import System.Exit (exitFailure)
import Data.Text.Lazy (unpack)
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap

assembleGlobals :: [Obj] -> IntMap Obj -> [Obj]
assembleGlobals globals storage = map restoreFunc globals
  where
    restoreFunc g = case IntMap.lookup (objKey g) storage of
      Just actualObj -> actualObj
      Nothing        -> g


pipeline file = do
  toks                             <- tokenize_ file
  (globals, toks, storage)         <- (parse . convert_keywords) toks
  (checkedGlobals, checkedStorage) <- scopecheck globals storage
  (checkedGlobals, checkedStorage) <- typecheck globals storage
  return (checkedGlobals, checkedStorage)

debugMode :: String -> Either Error String
debugMode file = do
  (checkedGlobals, checkedStorage) <- pipeline file

  let symTable = assembleGlobals checkedGlobals checkedStorage
  let dump = unpack (pShowNoColor symTable)
  return dump

compile :: String -> Either Error [String]
compile file = do
  (checkedGlobals, checkedStorage) <- pipeline file
  codegen checkedGlobals checkedStorage
