module Parse2 where
import RBCC
import Type
import Data.List



newtype Context = Context Token
  deriving (Show, Eq)

mkCtx :: Token -> Context
mkCtx = Context

data Declspec = DeclInt Context
              | DeclChar Context
              | DeclArr Declspec Int
              | DeclPtr Declspec
              | DeclFunc Declspec Context

              deriving (Show, Eq)

data VarDecl = Pointer Context String Declspec
             | Variable Context String Declspec
             deriving (Show, Eq)

data Decl = Function Context Declspec String
          | VariableDecl Context Declspec [VarDecl]
          deriving (Eq, Show)

newtype Program = Program [Decl] deriving (Eq, Show)

--TODO think about dubliaction type of var in Decl and VarDecl

mkVarDec ty varArr tok = VariableDecl (mkCtx tok) ty varArr
mkFuncDecl ty name tok = Function (mkCtx tok) ty name



headToken :: [Token] -> Either Error (Token, [Token])
headToken [] = Left $ ErrorText "Internal logic: headToken unreachable"
headToken (s : ss)  = Right (s , ss)



mkInt  = DeclInt . mkCtx
mkChar = DeclChar . mkCtx
mkFunc ty tok = DeclFunc ty (mkCtx tok)

-- declspec = "int" | "char"
declspec   :: [Token] -> Either Error (Declspec, [Token])
declspec toks = do
  (tok, toks) <- headToken toks

  case tokenKind tok of
    Keyword "int"  -> Right $  (mkInt tok, toks)
    Keyword "char" -> Right $  (mkChar tok, toks)
    _              -> Left  $ ErrorToken tok "Unkown type in declaration"


mkTokError tok err = Left $ ErrorToken tok err
mkTextError err = Left $ ErrorText err

skipTKind :: TokenKind -> [Token] -> Either Error [Token]
skipTKind kind [] = mkTextError $ "Unexpected skip of " ++ show kind
skipTKind kind (t : ts) = if tokenKind t == kind
                          then Right ts
                          else mkTokError t ("expected " ++ show kind)

skip = skipTKind

mkVar decl name tok = Variable (mkCtx tok) name decl

lookAhead :: [Token] -> Either Error Token
lookAhead toks = do
    (tok, _) <- headToken toks
    return tok

getNumber :: [Token] -> Either Error (Int, [Token])
getNumber toks = do
  (tok, toks) <- headToken toks
  case tokenKind tok of
      Num v -> return (v, toks)
      _ -> mkTokError tok "expected an number"

-- func-params = param ("," param)*
-- param       = declspec declarator
--funcParams :: Type -> Either Error Declspec
funcParams toks = do
  (args, toks) <- iter toks
  return (args, toks)
  where
    iter toks = do
      tok <- lookAhead toks
      case tokenKind tok of
        (Punct ")") -> do
          return ([], toks)
        _ -> do
          return ([], toks) --FIXME
          {-
          (basety, toks) <- declspec toks
          (arg, toks) <- declarator basety toks
          let res = params ++ [arg]
          tok <- lookAhead toks
          case tokenKind tok of
            (Punct ",") ->
              iter res
              return params
            _ -> return res
-}



-- type-suffix = (" func-params
--             | "[" num "]" type-suffix
--             | etc
typeSuffix :: Declspec -> [Token]-> Either Error (Declspec, [Token])
typeSuffix base toks = do
  tok <- lookAhead toks
  case tokenKind tok of
    Punct "(" -> do
      toks <- skip (Punct "(") toks
      (args, toks) <- funcParams toks
      toks <- skip (Punct ")") toks
      return ((DeclFunc base (mkCtx tok)), toks)
    Punct "[" -> do
      toks <- skipTKind (Punct "[") toks
      (len, toks) <- getNumber toks
      toks <- skipTKind (Punct "]") toks
      --node <- type_suffix base
      return (DeclArr base len, toks)
    _ -> return (base, toks)



-- declarator = "*"* ident type-suffix
declarator :: Declspec -> [Token] -> Either Error (VarDecl, [Token])
declarator basetype toks = do
  (ty, toks) <- ptrWrap basetype toks
  (tok, toks) <- headToken toks
  case tok of
    Token (Ident name) _ _ -> do
      (decl_type, toks) <- typeSuffix ty toks
      return $ (mkVar decl_type name tok, toks)
    _ -> mkTokError tok "expected an identifier"
  where
    ptrWrap base toks = do
      (tok, toks_) <- headToken toks
      case tokenKind tok of
        (Punct "*") -> ptrWrap (DeclPtr base) toks_
        _ -> return (base, toks)

tbd s = mkTextError $ "to be continued" ++ show s



globalVariable :: [Token] -> Either Error (Decl, [Token])
globalVariable toks = do
  fstTok <- lookAhead toks
  (ty, toks) <- declspec toks
  (vars, toks) <- iter ty toks []
  toks <- skipTKind (Punct ";") toks
  return (mkVarDec ty vars fstTok, toks)
  where
    iter ty toks acc = do
      (v, toks) <- declarator ty toks
      tok <- lookAhead toks
      case tokenKind tok of
        Punct "," -> do
          toks <- skipTKind (Punct ",") toks
          iter ty toks (acc ++ [v])
        _         -> pure (acc ++ [v], toks)



-- declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
declaration toks = do
  (tok, toks) <- headToken toks
  (basety, toks) <- declspec toks
--  nodes <- iter basety []
  toks <- skip (Punct ";") toks
  tbd "declaration"
{-
  add_type (BLOCK nodes) tok
  where
    decl_expr basety nodes = do
      (ty, name) <- declarator basety
      key <- new_lvar name ty
      isAssign <- head_equalM (Punct "=")
      if isAssign
      then do
        tok <- popHeadToken
        lhs <- add_type (VAR key) tok
        rhs <- assign
        node <- add_type (Assign lhs rhs) tok
        expression  <- add_type (EXPS_STMT node) tok
        return $ nodes ++ [expression]
      else return nodes
    iter basety nodes = do
      nodes_ <- decl_expr basety nodes
      isNext <- head_equalM (Punct ",")
      if isNext
      then do
        skip (Punct ",")
        decl_expr basety nodes_
      else return nodes_
-}



isFunction :: [Token] -> Either Error Bool
isFunction toks = do
  (tok, _) <- headToken toks
  case tokenKind tok of
    Punct ";" -> return False
    _ -> do
      (ty, toks) <- declspec toks
      (ftype, _) <- declarator ty toks
      case ftype of
        --FIXME: Function inside variable?? OMG???
        (Variable _ _ (DeclFunc _ _)) -> return True
        _            -> return False

--function :: ExceptT Error (State ParserState) ()
function toks = do
  (ty, toks) <- declspec toks
  (tok, _) <- headToken toks
  (ftype, toks) <- declarator ty toks

  toks <- skip (Punct "{") toks
  toks <- skip (Punct "}") toks
  return (mkFuncDecl ty "???" tok , toks)


  --s <- compound_stmt
  --let nodes = [s]






program2 :: [Token] -> Either Error (Program, [Token])
program2 [] = mkTextError "Unexpected end of program"
program2 toks@(s : ss) = case tokenKind s of
  EOF -> return (Program [], [])
  _ -> do
    isFunc <- isFunction toks
    if isFunc
    then do
      (func, toks) <- function toks
      (Program decl, _) <- program2 toks
      return (Program (func: decl) , toks)
    else do
      (vars, toks) <- globalVariable toks
      (Program decl, _) <- program2 toks
      return (Program (vars: decl) , toks)


parse2 :: [Token] -> Either Error Program
parse2 toks = do
  (prog, _) <- program2 toks
  return prog

