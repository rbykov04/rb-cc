--{-# LANGUAGE OverloadedStrings #-}
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

printErr err = unpack (pShow err)

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
            case compile prog of
              Left err -> expectationFailure (printErr (err))
              Right asm -> do
                writeFile "test_output.s" (unlines asm)
                let action = do
                      (ret, _ , cErr) <- readProcessWithExitCode "gcc" ["test_output.s", "-o", "test_prog"] ""
                      case ret of
                        ExitFailure c -> expectationFailure (printErr (cErr))
                        ExitSuccess -> do
                          (runExit, _ , _) <- readProcessWithExitCode "./test_prog" [] ""
                          case runExit of
                            ExitFailure code -> code `shouldBe` 42
                            ExitSuccess -> expectationFailure "run must retut some code"
                action `finally` do
                  removeFile "test_output.s"
                  removeFile "test_prog"
