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

vmCode :: String
vmCode = $(embedStringFile "src/runtime/vm.c")

printErr err = unpack (pShow err)


assertCompileAndRunBase compiler filename prog result = do
  case compiler prog of
    Left err -> expectationFailure (printErr (err))
    Right asm -> do
      let asmFile = filename ++ ".s"
      let exeFile = filename ++ ".exe"
      writeFile asmFile (unlines asm)
      let action = do
            (ret, _ , cErr) <- readProcessWithExitCode "gcc" [asmFile, "-o", exeFile] ""
            case ret of
              ExitFailure c -> expectationFailure ("for " ++ "filename" ++ (printErr (cErr)) ++ asmFile)
              ExitSuccess -> do
                (runExit, _ , _) <- readProcessWithExitCode ("./" ++ exeFile) [] ""
                case runExit of
                  ExitFailure code -> code `shouldBe` result
                  ExitSuccess -> expectationFailure $ "run must return some code for" ++ show prog
      action `finally` do
        removeFile asmFile
        removeFile exeFile

assertCompileAndRun    = assertCompileAndRunBase compile
assertCompileAndRunX86 = assertCompileAndRunBase compileX86


genVmC1 :: String
genVmC1 =  "int main(){"
       ++ "   int program[3];"
       ++ "   program[0] = 0;"
       ++ "   program[1] = 42;"
       ++ "   program[2] = 1;"
       ++ "   return vm_run(program, 3);"
       ++ "}"

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

diffTest prog resultMustBe= do
    it prog $ do
        case compileToTypedAST prog of
          Left err -> expectationFailure (printErr (err))
          Right (globals, storage) -> do
            -- FIXME : search main
            let ir = toIR (head (objBody (storage ! 1)))
            runVM ir `shouldBe` resultMustBe
            assertCompileAndRun "test_output" prog resultMustBe
            assertCompileAndRun "vm2" (vmCode ++ genVmC2 (ir)) resultMustBe
            assertCompileAndRunX86 "new_test" prog resultMustBe


spec :: Spec
spec = do
  describe "vm" $ do
    it "basicProg: genVmC2" $ do
        assertCompileAndRun "vm" (vmCode ++ genVmC1) 42
    diffTest "int main() { return 42; }" 42
    diffTest "int main() { return 22 + 20; }" 42
    diffTest "int main() { return 22 + 20 + 20; }" 62
    diffTest "int main() { return 22 + 20 - 21; }" 21
    diffTest "int main() { return 5 * 4 + 1; }" 21
    diffTest "int main() { return 5 * (4 + 1); }" 25
    diffTest "int main() { return (25 / 5) * (4 + 1); }" 25
