{-# LANGUAGE TemplateHaskell #-}
module VMSpec where

import Test.Hspec
import AST
import Error
import Tokenize
import Driver
import StackIsa
import VM
import StackCodegen
import Data.IntMap.Lazy
import Data.Either
import Data.List
import Data.Maybe
import Text.Pretty.Simple

import System.Environment
import System.IO
import System.Exit
import System.Directory
import System.Process
import Control.Exception (finally)
import Data.Text.Lazy (unpack)

import Data.FileEmbed (embedStringFile)

-- Эта строчка прочитает файл "runtime/vm.c" во время компиляции
-- и создаст переменную vmCode типа String.
vmCode :: String
vmCode = $(embedStringFile "src/runtime/vm.c")

printErr err = unpack (pShow err)

assertCompileAndRun filename prog result = do
  case compile prog of
    Left err -> expectationFailure (printErr (err))
    Right asm -> do
      let asmFile = filename ++ ".s"
      let exeFile = filename ++ ".exe"
      writeFile asmFile (unlines asm)
      let action = do
            (ret, _ , cErr) <- readProcessWithExitCode "gcc" [asmFile, "-o", exeFile] ""
            case ret of
              ExitFailure c -> expectationFailure ("for " ++ "filename" ++ (printErr (cErr)))
              ExitSuccess -> do
                (runExit, _ , _) <- readProcessWithExitCode ("./" ++ exeFile) [] ""
                case runExit of
                  ExitFailure code -> code `shouldBe` result
                  ExitSuccess -> expectationFailure "run must retut some code"
      action `finally` do
        removeFile asmFile
        removeFile exeFile


genVmC1 :: String
genVmC1 =  "int main(){"
       ++ "   int program[3];"
       ++ "   program[0] = 0;"
       ++ "   program[1] = 42;"
       ++ "   program[2] = 1;"
       ++ "   return vm_run(program, 3);"
       ++ "}"

gen :: [StackOp] -> [Int]
gen [] = []
gen (PushInt n : xs) = 0 : n : gen xs
gen (Ret       : xs) = 1 : gen xs

genVmC2 :: [StackOp] -> String
genVmC2 prog =
  let code = gen prog
      size = length code
      mapOp (idx, val) = "   program[" ++ show idx ++ "] = " ++ show val ++ ";\n"
      assignments = concatMap mapOp (zip [0..] code)
  in
  "int main(){"
       ++ "   int program[ " ++ show size ++  "];"
       ++ assignments
       ++ "   return vm_run(program, " ++ show size ++ ");"
       ++ "}"



spec :: Spec
spec = do
  describe "vm" $ do

    it "int main() { return 42; }" $ do
        let prog = "int main() { return 42; }"
        case compileToTypedAST prog of
          Left err -> expectationFailure (printErr (err))
          Right (globals, storage) -> do
            -- FIXME : search main
            let ir = toIR (head (objBody (storage ! 1)))
            ir `shouldBe` [PushInt 42, Ret]
            runVM ir `shouldBe` 42
            assertCompileAndRun "test_output" prog 42
            assertCompileAndRun "vm" (vmCode ++ genVmC1) 42
            assertCompileAndRun "vm2" (vmCode ++ genVmC2 (ir)) 42
