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
