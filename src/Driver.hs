module Driver where


import Codegen
import Tokenize
import Parse
import Semantic
import Error
import AST
import StackCodegen
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


compileToTypedAST :: String -> Either Error ([Obj], IntMap Obj)
compileToTypedAST file = do
  toks                             <- tokenize_ file
  ast                              <- (parse . convert_keywords) toks
  (checkedGlobals, checkedStorage) <- scopecheck ast
  (checkedGlobals, checkedStorage) <- typecheck checkedGlobals checkedStorage
  return (checkedGlobals, checkedStorage)

debugMode :: String -> Either Error String
debugMode file = do
  (checkedGlobals, checkedStorage) <- compileToTypedAST file

  let symTable = assembleGlobals checkedGlobals checkedStorage
  let dump = unpack (pShowNoColor symTable)
  return dump

compile :: String -> Either Error [String]
compile file = do
  (checkedGlobals, checkedStorage) <- compileToTypedAST file
  codegen checkedGlobals checkedStorage

compileX86 :: String -> Either Error [String]
compileX86 file = do
  (checkedGlobals, checkedStorage) <- compileToTypedAST file
  let ir = toIR (head (objBody (checkedStorage ! 1)))
  text <- codegenX86 ir
  return [text]
