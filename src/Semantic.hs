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

getVars :: ExceptT Error (State ParserState) (IntMap Obj)
getVars = do
  r <- get
  return $ allObjects r


get_var :: Int -> ExceptT Error (State ParserState) Obj
get_var key = do
  vars <- getVars
  return $ vars ! key



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


last' :: [a] -> a
last' ys = foldl1 (\_ -> \x -> x) ys


change_type new (Node kind (tok, _)) = Node kind (tok, new)

getLocals :: ExceptT Error (State ParserState) [Obj]
getLocals = do
  r <- get
  return $ (fst. currentVars) r

putLocals :: [Obj] -> ExceptT Error (State ParserState) ()
putLocals vars = do
  r <- get
  let obj = currentVars r
  put r { currentVars = (vars, snd obj)}

getGlobals :: ExceptT Error (State ParserState) [Obj]
getGlobals = do
  r <- get
  return $ (snd. currentVars) r

putGlobals :: [Obj] -> ExceptT Error (State ParserState) ()
putGlobals vars = do
  r <- get
  let obj = currentVars r
  put r { currentVars = (fst obj , vars)}

enterScope :: ExceptT Error (State ParserState) ()
enterScope = do
  r <- get
  put r {scopes = (Scope [] : scopes r)}

leaveScope :: ExceptT Error (State ParserState) ()
leaveScope = do
  r <- get
  case scopes r of
    [] -> put r
    (_: tail_scopes) -> put r {scopes = tail_scopes}

newUniqName :: ExceptT Error (State ParserState) String
newUniqName = do
  counter <- gets uniqCounter
  modify (\s -> s {uniqCounter = counter + 1})
  return $ ".L.." ++ show counter



pushScope :: String -> Obj -> ExceptT Error (State ParserState) ()
pushScope name var = do
  r <- get
  case scopes r of
    [] ->  throwE (ErrorText "Please enter to scope before")
    ((Scope vars) : tail_scopes) -> do
      let sc = VarScope name var
      let newScopes = Scope (sc : vars): tail_scopes
      put r {scopes = newScopes}


putVars :: IntMap Obj -> ExceptT Error (State ParserState) ()
putVars vars = modify (\s -> s {allObjects = vars})

add_type_pure :: Node_ Typed -> Token -> Either Error (Node Typed)
add_type_pure node tok = case node of
  Assign lhs _ ->
    let ty = nodeType lhs
    in Right (Node node (tok, ty))

  BIN_OP _ lhs _ -> do
    let ty = nodeType lhs
    return $ Node node (tok, ty)
  _ ->  Left $ ErrorToken tok "not implemented yet"



add_type nodeKind tok = case nodeKind of
  UNARY op node -> case op of
    Addr -> case (typeKind. nodeType) node of
      ARRAY base _ -> do
        let ty = pointer_to base
        return $ Node nodeKind (tok, ty)
      _              -> do
        let ty = (pointer_to . nodeType) node
        return $ Node nodeKind (tok, ty)

    Deref -> case (typeKind. nodeType) node of
      ARRAY base _ -> do
        return $ Node nodeKind (tok, base)
      PTR base -> do
        return $ Node nodeKind (tok, base)
      _ -> throwE (ErrorToken tok "invalid pointer dereference")


    _    -> do
      let ty = nodeType node
      return $ Node nodeKind (tok, ty)

  BIN_OP _ lhs _ -> throwE (ErrorToken tok "internal error: deprecated")
  Assign lhs _ -> throwE (ErrorToken tok "internal error: deprecated")

  VAR key -> do
    obj <- get_var key
    return $ Node nodeKind (tok , (objType obj))

  STMT_EXPR body -> do
    case body of
      Node (BLOCK blocks) _ -> do
        case (last' blocks) of
          Node (EXPS_STMT x) (tt, _) -> do
            let ty = nodeType x
            --throwE (ErrorToken tok ("types: " ++ show x))
            return $ Node nodeKind (tt , ty)
      _ -> throwE (ErrorToken tok "statement expression returning void is not supported")

  _ -> return $ Node nodeKind (tok, make_int)




make_offset ty count tok = do
  base <- add_type (NUM ((typeSize) ty)) tok
  add_type (BIN_OP Mul count base) tok

ptr_math op ptr ty count token = do
  offset <- make_offset ty count token
  add_type (BIN_OP op ptr offset) token


new_add  :: Node Typed ->Node Typed ->Token -> ExceptT Error (State ParserState) (Node Typed)
new_add lhs rhs tok
  | is_integer (nodeType rhs) && is_integer (nodeType lhs) = add_type (BIN_OP Add lhs rhs) tok
  | otherwise =  case ((get_ptr_base . nodeType) lhs, (get_ptr_base . nodeType) rhs) of

  ((Just tyBase), Nothing)  -> ptr_math Add lhs tyBase rhs tok
  (Nothing, (Just tyBase))  -> ptr_math Add rhs tyBase lhs tok

  -- ptr +  ptr
  _ ->  throwE (ErrorToken tok "invalid operands")

new_sub  :: Node Typed ->Node Typed ->Token -> ExceptT Error (State ParserState) (Node Typed)
new_sub lhs rhs tok
  | is_integer (nodeType rhs) && is_integer (nodeType lhs) = add_type (BIN_OP Sub lhs rhs) tok
  | otherwise =  case ((get_ptr_base . nodeType) lhs, (get_ptr_base . nodeType) rhs) of
    ((Just tyBase), Nothing)  -> ptr_math Sub lhs tyBase rhs tok

  -- ptr - ptr, which returns how many elements are between the two.
    ((Just tyBase), (Just _)) -> ptr_diff lhs tyBase rhs tok
  -- num - ptr
    _ -> throwE (ErrorToken tok "invalid operands")
    where
      ptr_diff a atype b token = do
        base <- add_type (NUM (typeSize atype)) token
        num <- add_type (BIN_OP Sub a b) tok
        add_type (BIN_OP Div (change_type make_int num) base) tok

find_var_in :: String -> [Scope] -> ExceptT Error (State ParserState) (Maybe Obj)
find_var_in _ [] = do
  return Nothing
find_var_in name (Scope [] : sc) = find_var_in name sc
find_var_in name ((Scope (v : vars)): sc_tail) = do
  if scopeName v == name then do
    return (Just ( scopeObj v))
  else do
    find_var_in name ((Scope (vars)): sc_tail)



find_var :: String -> ExceptT Error (State ParserState) (Maybe Obj)
find_var name = do
  r <- get
  case scopes r of
    [] -> do
      return Nothing
    sc -> do
      find_var_in name sc
--      throwE (ErrorText ("find var in " ++ show sc))


find_var' :: String -> ExceptT Error (State ParserState) (Maybe Obj)
find_var' var = do
  gvars <- getGlobals
  lvars <- getLocals

  return $ find f (gvars ++ lvars)
  where
    f obj = var == objName obj


new_var :: String -> Type -> Bool -> ExceptT Error (State ParserState) Obj
new_var name t isLocal = do
  vars <- getVars
  let key = IntMap.size vars + 1
  let v = Obj key name t 0 isLocal [] [] Nothing

  putVars (IntMap.insert key v vars)
  pushScope name v
  return v

update_var :: (Obj -> Obj) ->Int -> ExceptT Error (State ParserState) ()
update_var f key= do
  vars <- getVars
  let vars' = IntMap.adjust f key vars
  putVars vars'

new_lvar :: String -> Type -> ExceptT Error (State ParserState) Int
new_lvar name t = do
  v <- new_var name t True
  vars <- getLocals
  putLocals (v:vars)
  return $ objKey v

new_gvar :: String -> Type -> ExceptT Error (State ParserState) Int
new_gvar name t = do
  v <- new_var name t False
  vars <- getGlobals
  putGlobals (v:vars)
  return $ objKey v

new_anon_gvar :: Type -> ExceptT Error (State ParserState) Int
new_anon_gvar t = do
  name <- newUniqName
  new_gvar name t

new_string_literal text ty= do
  key <- new_anon_gvar ty
  update_var (updateFunc text) key
  return key
  where
    updateFunc data_ obj = obj {objInitData = (Just data_)};

checkObj :: IntMap Obj -> Obj -> Either Error Obj
checkObj storage obj = do
  checkedBody <- mapM (checkNode storage) (objBody obj)
  return $ obj { objBody = checkedBody }

checkNode :: IntMap Obj -> Node Typed -> Either Error (Node Typed)
checkNode storage node@(Node nodeKind' (tok, ty)) = case nodeKind' of
  Assign lhs rhs -> do
    tLhs <- checkNode storage lhs
    tRhs <- checkNode storage rhs
    add_type_pure (Assign tLhs tRhs) tok

  BIN_OP op lhs rhs -> do
    tLhs <- checkNode storage lhs
    tRhs <- checkNode storage rhs
    add_type_pure (BIN_OP op tLhs tRhs) tok
  _ -> return node


typecheck :: [Obj] -> IntMap Obj -> Either Error [Obj]
typecheck globals storage = mapM (checkObj storage) globals
