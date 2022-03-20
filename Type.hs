module Type where
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Environment
import System.IO
import RBCC
import Codegen
import Tokenize
import Data.List

size_of (Type _ size) = size
array_of base len = Type (ARRAY base len) (size_of base * len)
func_type ret_type args = Type (FUNC ret_type args) 8
pointer_to base = Type (PTR base) 8
make_int = Type INT 8

add_type nodeKind tok = case nodeKind of
  UNARY op node -> case op of
    Addr -> case (typeKind. nodeType) node of
      ARRAY base _ -> do
        let ty = pointer_to base
        return $ Node nodeKind ty tok
      _              -> do
        let ty = (pointer_to . nodeType) node
        return $ Node nodeKind ty tok

    Deref -> case (typeKind. nodeType) node of
      ARRAY base _ -> do
        return $ Node nodeKind base tok
      PTR base -> do
        return $ Node nodeKind base tok
      _ -> throwE (ErrorToken tok "invalid pointer dereference")


    _    -> do
      let ty = nodeType node
      return $ Node nodeKind ty tok

  BIN_OP _ lhs _ -> do
    let ty = nodeType lhs
    return $ Node nodeKind ty tok

  Assign lhs _ -> do
    let ty = nodeType lhs
    return $ Node nodeKind ty tok

  VAR (Obj _ ty _) -> do
    return $ Node nodeKind ty tok

  _ -> return $ Node nodeKind make_int tok

change_type new (Node kind _ tok) = Node kind new tok