module Type where
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Environment
import System.IO
import RBCC
import Tokenize
import Error
import Data.List

size_of (Type _ size) = size
array_of base len = Type (ARRAY base len) (size_of base * len)
func_type ret_type args = Type (FUNC ret_type args [] (-99)) 8
pointer_to base = Type (PTR base) 8
make_int  = Type INT 8
make_char = Type CHAR 1

is_integer :: Type -> Bool
is_integer t = case typeKind t of
  CHAR -> True
  INT -> True
  _   -> False

get_ptr_base :: Type -> Maybe Type
get_ptr_base t = case typeKind t of
  PTR   base -> Just base
  ARRAY base _ -> Just base
  _   -> Nothing


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

  VAR key -> do
    obj <- get_var key
    return $ Node nodeKind (objType obj) tok

  STMT_EXPR body -> do
    case body of
      Node (BLOCK blocks) _ _ -> do
        case (last' blocks) of
          Node (EXPS_STMT x) _ tt -> do
            let ty = nodeType x
            --throwE (ErrorToken tok ("types: " ++ show x))
            return $ Node nodeKind ty tt
      _ -> throwE (ErrorToken tok "statement expression returning void is not supported")

  _ -> return $ Node nodeKind make_int tok

last' :: [a] -> a
last' ys = foldl1 (\_ -> \x -> x) ys



change_type new (Node kind _ tok) = Node kind new tok
