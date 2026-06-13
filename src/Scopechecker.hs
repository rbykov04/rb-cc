module Scopechecker where
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Environment
import System.IO
import AST
import Tokenize
import Semantic
import Error
import Data.List
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap

data ScopeCheckerState = ScopeCheckerState
  { currentVars :: ([Obj], [Obj])
  , allObjects  :: IntMap Obj
  , uniqCounter :: Int
  , scopes      :: [Scope]
  } deriving (Show)


getVars :: ExceptT Error (State ScopeCheckerState) (IntMap Obj)
getVars = do
  r <- get
  return $ allObjects r


get_var :: Int -> ExceptT Error (State ScopeCheckerState) Obj
get_var key = do
  vars <- getVars
  return $ vars ! key


putLocals :: [Obj] -> ExceptT Error (State ScopeCheckerState) ()
putLocals vars = do
  r <- get
  let obj = currentVars r
  put r { currentVars = (vars, snd obj)}

getGlobals :: ExceptT Error (State ScopeCheckerState) [Obj]
getGlobals = do
  r <- get
  return $ (snd. currentVars) r

putGlobals :: [Obj] -> ExceptT Error (State ScopeCheckerState) ()
putGlobals vars = do
  r <- get
  let obj = currentVars r
  put r { currentVars = (fst obj , vars)}

enterScope :: ExceptT Error (State ScopeCheckerState) ()
enterScope = do
  r <- get
  put r {scopes = (Scope [] : scopes r)}

leaveScope :: ExceptT Error (State ScopeCheckerState) ()
leaveScope = do
  r <- get
  case scopes r of
    [] -> put r
    (_: tail_scopes) -> put r {scopes = tail_scopes}

newUniqName :: ExceptT Error (State ScopeCheckerState) String
newUniqName = do
  counter <- gets uniqCounter
  modify (\s -> s {uniqCounter = counter + 1})
  return $ ".L.." ++ show counter

getLocals :: ExceptT Error (State ScopeCheckerState) [Obj]
getLocals = do
  r <- get
  return $ (fst. currentVars) r


pushScope :: String -> Obj -> ExceptT Error (State ScopeCheckerState) ()
pushScope name var = do
  r <- get
  case scopes r of
    [] ->  throwE (ErrorText "Please enter to scope before")
    ((Scope vars) : tail_scopes) -> do
      let sc = VarScope name var
      let newScopes = Scope (sc : vars): tail_scopes
      put r {scopes = newScopes}


putVars :: IntMap Obj -> ExceptT Error (State ScopeCheckerState) ()
putVars vars = modify (\s -> s {allObjects = vars})




find_var_in :: String -> [Scope] -> ExceptT Error (State ScopeCheckerState) (Maybe Obj)
find_var_in _ [] = do
  return Nothing
find_var_in name (Scope [] : sc) = find_var_in name sc
find_var_in name ((Scope (v : vars)): sc_tail) = do
  if scopeName v == name then do
    return (Just ( scopeObj v))
  else do
    find_var_in name ((Scope (vars)): sc_tail)



find_var :: String -> ExceptT Error (State ScopeCheckerState) (Maybe Obj)
find_var name = do
  r <- get
  case scopes r of
    [] -> do
      return Nothing
    sc -> do
      find_var_in name sc
--      throwE (ErrorText ("find var in " ++ show sc))


find_var' :: String -> ExceptT Error (State ScopeCheckerState) (Maybe Obj)
find_var' var = do
  gvars <- getGlobals
  lvars <- getLocals

  return $ find f (gvars ++ lvars)
  where
    f obj = var == objName obj


new_var :: String -> Type Typed -> Bool -> ExceptT Error (State ScopeCheckerState) Obj
new_var name t isLocal = do
  vars <- getVars
  let key = IntMap.size vars + 1
  let v = Obj key name t 0 isLocal [] [] Nothing

  putVars (IntMap.insert key v vars)
  pushScope name v
  return v

update_var :: (Obj -> Obj) ->Int -> ExceptT Error (State ScopeCheckerState) ()
update_var f key= do
  vars <- getVars
  let vars' = IntMap.adjust f key vars
  putVars vars'

new_lvar :: String -> Type Typed -> ExceptT Error (State ScopeCheckerState) Int
new_lvar name t = do
  v <- new_var name t True
  vars <- getLocals
  putLocals (v:vars)
  return $ objKey v

new_gvar :: String -> Type Typed -> ExceptT Error (State ScopeCheckerState) Int
new_gvar name t = do
  v <- new_var name t False
  vars <- getGlobals
  putGlobals (v:vars)
  return $ objKey v

new_anon_gvar :: Type Typed -> ExceptT Error (State ScopeCheckerState) Int
new_anon_gvar t = do
  name <- newUniqName
  new_gvar name t

new_string_literal text ty= do
  key <- new_anon_gvar ty
  update_var (updateFunc text) key
  return key
  where
    updateFunc data_ obj = obj {objInitData = (Just data_)};



create_param_lvars :: [(Type Typed, String)] -> ExceptT Error (State ScopeCheckerState) [Int]
create_param_lvars = mapM $ (uncurry . flip) new_lvar


scopedNode :: Node Parsed -> Node_ Typed -> ExceptT Error (State ScopeCheckerState) (Node Typed)
scopedNode node kind =
  let tok = nodeExt node
      ty  = (Type INT (-99))
  in return $ Node kind (tok, ty)

scopeCheckNode :: Node Parsed -> ExceptT Error (State ScopeCheckerState) (Node Typed)
scopeCheckNode node = case nodeNode node of
  NUM v -> scopedNode node (NUM v)
  VAR name -> do
    fv <- find_var name
    case fv of
      Nothing -> throwE (ErrorScope node "undefined variable")
      Just var -> scopedNode node (VAR (objKey var))
  RETURN body -> do
    checkedBody <- scopeCheckNode body
    scopedNode node (RETURN checkedBody)
  BLOCK body -> do
    checkedBody <- mapM scopeCheckNode body
    scopedNode node (BLOCK checkedBody)
  EXT (FUNCTION name unType body) -> throwE (ErrorScope node "Function can be define only on top level")
  _ -> throwE (ErrorScope node "this is not supported")


upgradeType :: Type Parsed -> ExceptT Error (State ScopeCheckerState) (Type Typed)
upgradeType ty = case typeKind ty of
  INT -> return $ make_int
  CHAR -> return $ make_char
  PTR unBase -> do
    tyBase <- upgradeType unBase
    return $ pointer_to tyBase
  ARRAY unBase len -> do
    tyBase <- upgradeType unBase
    return $ array_of tyBase len
  FUNC unRet unParams -> do
    tyRet <- upgradeType unRet
    tyParams <- mapM upgradeParam unParams
    args <- create_param_lvars tyParams
    return $ func_type tyRet args
    where
      upgradeParam (unParam, name) = do
        tyParam <- upgradeType unParam
        return (tyParam , name)



topLevel :: [Node Parsed] -> ExceptT Error (State ScopeCheckerState) ()
topLevel nodes = mapM_ checkTopLevel nodes
  where
    checkTopLevel :: Node Parsed -> ExceptT Error (State ScopeCheckerState) ()
    checkTopLevel node = case nodeNode node of
      EXT (DECL_VAR name unType) -> do
        tyType <- upgradeType unType
        key <- new_gvar name tyType
        return ()
      EXT (FUNCTION name unType body) -> do
        tyType <- upgradeType unType
        key <- new_gvar name tyType

        enterScope
        putLocals []
        checkedBody <- scopeCheckNode body
        locals <- getLocals
        update_var (updateFunc (map objKey locals) [checkedBody]) key
        leaveScope

      _ -> throwE (ErrorScope node "On top level, global variables or function are expected")
    updateFunc locals nodes obj = obj {objLocals = locals, objBody = nodes};

scopecheck :: [Node Parsed] -> Either Error ([Obj], IntMap Obj)
scopecheck ast = do
  let initState = ScopeCheckerState
        { currentVars = ([], [])
        , allObjects  = IntMap.empty
        , uniqCounter = 0
        , scopes      = [Scope []]
        }
  let (r, newState) = runState (runExceptT (topLevel ast)) initState
  let globals = (snd . currentVars)  newState
  let storage = allObjects newState
  case r of
    Left e -> Left e
    Right _ -> return (globals, storage)
