--{-# LANGUAGE OverloadedStrings #-}
module ScopescheckerSpec where

import Test.Hspec
import AST
import Error
import Tokenize
import Parse
import Scopechecker
import Data.IntMap.Lazy
import Data.Either
import Data.List
import Data.Maybe

runPurePipeline :: String -> Either Error ([Obj], IntMap Obj)
runPurePipeline sourceCode = do
  tokens <- tokenize_ sourceCode
  ast    <- (parse . convert_keywords) tokens
  scopecheck ast

spec :: Spec
spec = do
  describe "Scopechecker Pure Integration Tests" $ do

    context "Happy Path: Valid code structures" $ do
      it "allows global variables to be accessed inside functions" $ do
        let code = "int x; int main() { return x; }"
        case runPurePipeline code of
          Left err -> expectationFailure $ "compile is failed: " ++ show err
          Right (globals, _) -> do
            let mGvar = find (\o -> objName o == "x") globals
            case mGvar of
                Just xVar -> do
                        objIsLocal xVar `shouldBe` False
                        typeKind (objType xVar) `shouldBe` INT
                Nothing -> expectationFailure "Global 'x' has not been founded!"
{-
      it "allows local variables within their native block scope" $ do
        let code = "int main() { int y = 5; return y; }"
        runPurePipeline code `shouldSatisfy` isRight

    context "Block Scoping ({ }) and Variable Shadowing" $ do
      it "allows an inner block to read variables from a parent scope" $ do
        let code = "int main() { int x = 1; { int y = x; } return 0; }"
        runPurePipeline code `shouldSatisfy` isRight

      it "supports variable shadowing inside nested code blocks" $ do
        -- 'x' is redefined in an inner scope. The outer 'x' is shadowed.
        let code = "int main() { int x = 1; { char *x = \"a\"; } return x; }"
        runPurePipeline code `shouldSatisfy` isRight

    context "Negative Cases: Scope isolation errors" $ do
      it "fails when trying to access an inner-block variable from an outer scope" $ do
        -- 'inner' goes out of scope after the closing brace, causing an error
        let code = "int main() { { int inner = 5; } return inner; }"
        runPurePipeline code `shouldSatisfy` isLeft

      it "fails when accessing variables between independent sibling blocks" $ do
        -- Sibling blocks are completely isolated from each other
        let code = "int main() { { int a = 2; } { a = 3; } return 0; }"
        runPurePipeline code `shouldSatisfy` isLeft

      it "fails when functions try to access local variables of other functions" $ do
        -- Scope isolation at the function level
        let code = "void foo() { int secret = 42; } int main() { return secret; }"
        runPurePipeline code `shouldSatisfy` isLeft

    context "Error Handling: Redefinitions" $ do
      it "fails on variable redefinition within the same precise scope level" $ do
        -- Error: declaring the same variable twice in the exact same block
        let code = "int main() { int a = 1; int a = 2; return 0; }"
        runPurePipeline code `shouldSatisfy` isLeft

-}
