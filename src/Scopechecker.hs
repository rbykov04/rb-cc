module Scopechecker where
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

data ScopeCheckerState = ScopeCheckerState
  { currentVars_ :: ([Obj], [Obj])
  , allObjects_  :: IntMap Obj
  , uniqCounter_ :: Int
  , scopes_      :: [Scope]
  } deriving (Show)


getVars :: ExceptT Error (State ParserState) (IntMap Obj)
getVars = do
  r <- get
  return $ allObjects r


get_var :: Int -> ExceptT Error (State ParserState) Obj
get_var key = do
  vars <- getVars
  return $ vars ! key


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

getLocals :: ExceptT Error (State ParserState) [Obj]
getLocals = do
  r <- get
  return $ (fst. currentVars) r


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



create_param_lvars :: [(Type,String)] -> ExceptT Error (State ParserState) [Int]
create_param_lvars = mapM $ (uncurry . flip) new_lvar


checkNode :: Node Parsed -> ExceptT Error (State ScopeCheckerState) ()
checkNode  = error "?"

scopechecker_ :: [Node Parsed] -> ExceptT Error (State ScopeCheckerState) ()
scopechecker_  nodes = mapM_ checkNode nodes

scopecheck :: [Node Parsed] -> Either Error ([Obj], IntMap Obj)
scopecheck ast = do
  let initState = ScopeCheckerState
        { currentVars_ = ([], [])
        , allObjects_  = IntMap.empty
        , uniqCounter_ = 0
        , scopes_      = []
        }
  let (r, newState) = runState (runExceptT (scopechecker_ ast)) initState
  let globals = (snd . currentVars_)  newState
  let storage = allObjects_ newState
  case r of
    Left e -> Left e
    Right _ -> return (globals, storage)
