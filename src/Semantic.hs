module Semantic where
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Environment
import System.IO
import AST
import Tokenize
import Error
import Data.List

import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap



size_of (Type _ size) = size
array_of base len = Type (ARRAY base len) (size_of base * len)
func_type ret_type args = Type (FUNC ret_type (args , [] , (-99))) 8
pointer_to base = Type (PTR base) 8
make_int  = Type INT 8
make_untyped  = Type INT (-99)
make_char = Type CHAR 1

is_integer :: Type Typed -> Bool
is_integer t = case typeKind t of
  CHAR -> True
  INT -> True
  _   -> False

get_ptr_base :: Type Typed -> Maybe (Type Typed)
get_ptr_base t = case typeKind t of
  PTR   base -> Just base
  ARRAY base _ -> Just base
  _   -> Nothing


last' :: [a] -> a
last' ys = foldl1 (\_ -> \x -> x) ys


change_type new (Node kind (tok, _)) = Node kind (tok, new)

get_base_for_deref _ (ARRAY base _) = return base
get_base_for_deref _ (PTR base)   = return base
get_base_for_deref tok node = Left (ErrorToken tok ("invalid pointer dereference, for " ++ show node))


add_typed_node :: IntMap Obj -> Node_ Typed -> Token -> Either Error (Node Typed)
add_typed_node storage nodeKind tok = case nodeKind of

  VAR key -> case IntMap.lookup key storage of
    Just obj -> return $ Node nodeKind (tok, objType obj)
    Nothing  -> Left $ ErrorToken tok ("Semantic Error: unknown variable ID " ++ show key)

  UNARY op node -> case op of
    Addr ->
      let ty = case (typeKind. nodeType) node of
            ARRAY base _ -> pointer_to base
            _            -> pointer_to (nodeType node)
      in return $ Node nodeKind (tok, ty)
    Deref -> do
      let kind = (typeKind . nodeType) node
      base <- get_base_for_deref tok kind
      return $ Node nodeKind (tok, base)
    _ -> return $ Node nodeKind (tok, nodeType node)

  Assign lhs _ ->
    let tyL = nodeType lhs
    in Right (Node nodeKind (tok, tyL))

  BIN_OP op lhs _ ->
    let tyL = nodeType lhs

        resType = case op of
          ND_EQ -> make_int
          ND_NE -> make_int
          ND_LT -> make_int
          ND_LE -> make_int
          _     -> tyL

    in Right (Node nodeKind (tok, tyL))

  SIZEOF _ -> Left $ ErrorToken tok "Compiler Bug: SIZEOF node must be transformed into NUM in checkNode before reaching add_typed_node!"
  _ ->  Right (Node nodeKind (tok, make_int))


checkMakeOffset storage ty count tok = do
  base <- add_typed_node storage (NUM (typeSize ty)) tok
  add_typed_node storage (BIN_OP Mul count base) tok

checkPtrMath storage op ptr ty count token = do
  offset <- checkMakeOffset storage ty count token
  add_typed_node storage (BIN_OP op ptr offset) token

checkNodeForAdd storage lhs rhs tok
    | is_integer (nodeType rhs) && is_integer (nodeType lhs) = add_typed_node storage (BIN_OP Add lhs rhs) tok
    | otherwise =
      case ((get_ptr_base . nodeType) lhs, (get_ptr_base . nodeType) rhs) of
        ((Just tyBase), Nothing)  -> checkPtrMath storage Add lhs tyBase rhs tok
        (Nothing, (Just tyBase))  -> checkPtrMath storage Add rhs tyBase lhs tok
        -- ptr +  ptr
        _ ->  Left (ErrorToken tok "invalid operands")

checkNodeForSub storage lhs rhs tok
    | is_integer (nodeType rhs) && is_integer (nodeType lhs) = add_typed_node storage (BIN_OP Sub lhs rhs) tok
    | otherwise =
      case ((get_ptr_base . nodeType) lhs, (get_ptr_base . nodeType) rhs) of
            ((Just tyBase), Nothing)  -> checkPtrMath storage Sub lhs tyBase rhs tok
            -- ptr - ptr, which returns how many elements are between the two.
            ((Just tyBase), (Just _)) -> ptr_diff storage lhs tyBase rhs tok
            -- num - ptr
            _ -> Left (ErrorToken tok "invalid operands")
    where
      ptr_diff storage a atype b token = do
        base <- add_typed_node storage (NUM (typeSize atype)) token
        num <- add_typed_node storage (BIN_OP Sub a b) tok
        add_typed_node storage (BIN_OP Div (changeType make_int num) base) tok

      changeType new (Node kind (tok, _)) = Node kind (tok, new)



checkNode :: IntMap Obj -> Node Typed -> Either Error (Node Typed)
checkNode storage (Node nodeKind' (tok, ty)) = case nodeKind' of
  NUM v       -> add_typed_node storage (NUM v) tok
  VAR v       -> add_typed_node storage (VAR v) tok

  Assign lhs rhs -> do
    tLhs <- checkNode storage lhs
    tRhs <- checkNode storage rhs
    add_typed_node storage (Assign tLhs tRhs) tok

  BIN_OP op lhs rhs -> do
    tLhs <- checkNode storage lhs
    tRhs <- checkNode storage rhs
    case op of
      Add -> checkNodeForAdd storage tLhs tRhs tok
      Sub -> checkNodeForSub storage tLhs tRhs tok
      _   -> add_typed_node storage (BIN_OP op tLhs tRhs) tok

  SIZEOF n -> do
    tNode <- checkNode storage n
    add_typed_node storage (NUM ((size_of . nodeType) tNode)) tok


  UNARY op n -> do
    tNode <- checkNode storage n
    add_typed_node storage (UNARY op tNode) tok

  EXPS_STMT n -> do
    tNode <- checkNode storage n
    add_typed_node storage (EXPS_STMT tNode) tok

  STMT_EXPR body -> do
    tBody <- checkNode storage body
    add_typed_node storage (STMT_EXPR tBody) tok

  FUNCALL name list -> do
    checkedElements <- mapM (checkNode storage) list
    add_typed_node storage (FUNCALL name checkedElements) tok

  RETURN n    -> do
    tRet <- checkNode storage n
    add_typed_node storage (RETURN tRet) tok

  BLOCK list -> do
    checkedElements <- mapM (checkNode storage) list
    add_typed_node storage (BLOCK checkedElements) tok

  IF unCond  unThen unElse   -> do
    tCond <- checkNode storage unCond
    tThen <- checkNode storage unThen
    tElse <- traverse (checkNode storage) unElse
    add_typed_node storage (IF tCond tThen tElse) tok

  FOR unInit unCond unInc unBody -> do
    tInit <- traverse (checkNode storage) unInit
    tCond <- traverse (checkNode storage) unCond
    tInc <- traverse (checkNode storage) unInc
    tBody <- checkNode storage unBody
    add_typed_node storage(FOR tInit tCond tInc tBody) tok


checkObj storage (key, obj) = do
  let res = mapM (checkNode storage) (objBody obj)
  case res of
    Left e -> case e of
      ErrorToken tok text -> Left $ ErrorType tok text obj
      _                   -> Left e
    Right checkedBody -> return (key, obj { objBody = checkedBody })



typecheck :: [Obj] -> IntMap Obj -> Either Error ([Obj], IntMap Obj)
typecheck globals storage = do
  let objs = IntMap.toList storage

  checkedPairs <- mapM (checkObj storage) objs

  let updatedStorage = IntMap.fromList checkedPairs
  let updatedGlobals = map (\g -> case IntMap.lookup (objKey g) updatedStorage of
                                    Just realObj -> realObj
                                    Nothing      -> g
                           ) globals
  return (updatedGlobals , updatedStorage)
