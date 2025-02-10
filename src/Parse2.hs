module Parse2 where
--import RBCC
import Error
import Tokenize
import Data.List



newtype Context = Context Token
  deriving (Show, Eq)

mkCtx :: Token -> Context
mkCtx = Context

data Expr = Prim Primary deriving (Show, Eq)
data Stmt = Return Context Expr
          | Block Context [Stmt]
          deriving (Show, Eq)



data Declspec = DeclInt Context
              | DeclChar Context
              | DeclArr Declspec Int
              | DeclPtr Declspec
              | DeclFunc Declspec Context

              deriving (Show, Eq)

data VarDecl = Pointer Context String Declspec
             | Variable Context String Declspec
             deriving (Show, Eq)

data Decl = Function Context Declspec String [Stmt]
          | VariableDecl Context Declspec [VarDecl]
          deriving (Eq, Show)

newtype Program = Program [Decl] deriving (Eq, Show)

--TODO think about dubliaction type of var in Decl and VarDecl

mkVarDec ty varArr tok = VariableDecl (mkCtx tok) ty varArr
mkFuncDecl ty name nodes tok = Function (mkCtx tok) ty name nodes



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

isType toks = do
  tok <- lookAhead toks
  case tokenKind tok of
    (Keyword "int")  -> return True
    (Keyword "char") -> return True
    _                -> return False

parserIF :: ([Token] -> Either Error Bool)
         -> ([Token] -> Either Error (a, [Token]))
         -> ([Token] -> Either Error (a, [Token]))
         ->
        [Token]
         -> Either Error (a, [Token])
parserIF cond thn els toks = do
    res <- cond toks
    if res
    then thn toks
    else els toks

assign toks = do
  tbd "assign"
  {-
  lhs <- equality
  kind <- seeHeadTokenKind
  case kind of
    Punct "=" -> do
      tok <- popHeadToken
      rhs <- assign
      add_type (Assign lhs rhs) tok
    _ -> return lhs
-}

data Primary = Number Context Int
  deriving (Eq, Show)

mkNumber num tok = Number (mkCtx tok) num

-- primary = "(" "{" stmt+ "}" ")"
--         | "(" expr ")"
--         | "sizeof" unary
--         | ident func-args?
--         | str
--         | num
-- args = "(" ")"
primary toks = do
  (tok, toks) <- headToken toks
  case tokenKind tok of
    Num v -> do
      return (mkNumber v tok, toks)
    _ -> tbd " primary"
        {-
    Str str -> do
          let ty = array_of make_char (length str + 1)
          o <- new_string_literal (str ++ "\0") ty
          add_type (VAR o) t
        Ident str -> do
          next_kind <- seeHeadTokenKind
          case next_kind of
            Punct "(" -> do
              putTokens (t:ts)
              funcall
            _ -> do
              fv <- find_var str
              case fv of
                Nothing -> throwE (ErrorToken t "undefined variable")
                Just var -> add_type (VAR (objKey var)) t
        Punct "(" -> do
          isGnuStatementExpression <- head_equalM (Punct "{")
          if isGnuStatementExpression
          then do
          -- This is a GNU statement expresssion.
            skip (Punct "{")
            body <- compound_stmt
            skip (Punct ")")
            node <- add_type (STMT_EXPR body) t
            return node
          else do
            node <- expr
            skip (Punct ")")
            return node
        Keyword "sizeof" -> do
          node <- unary
          add_type (NUM ((size_of . nodeType) node)) t
        _ -> throwE (ErrorToken t "expected an expression")
    [] -> error "not achivable"
-}



expr  toks     = do
  (p, toks) <- primary toks
  return (Prim p, toks)
--expr       = assign


--stmt = "return" expr ";"
--     | "if" "(" expr ")" stmt ("else" stmt)?
--     | "for" "(" expr-stmt expr? ";" expr? ")" stmt
--     | "while" "(" expr" )" stmt
--     | "{" compound-stmt
--     | expr-stmt

stmt toks = do
  (tok, toks) <- headToken toks
  case tokenKind tok of
    (Keyword "return") -> do
      (node, toks) <- expr toks
      toks <- skip (Punct ";") toks
      return (Return (mkCtx tok) node, toks)
    _ -> tbd $ "stmt" ++ show tok
    {-
  else if head_equal ts (Keyword "while")
  then do
    tok <- popHeadToken
    skip (Punct "(")
    cond <- expr
    skip (Punct ")")
    body <- stmt
    add_type (FOR Nothing (Just cond) Nothing body) tok
  else if head_equal ts (Keyword "for")
  then do
    tok <- popHeadToken
    skip (Punct "(")

    ini <- expr_stmt
    cond <- maybe_expr (Punct ";")
    skip (Punct ";")

    inc<- maybe_expr (Punct ")")
    skip (Punct ")")

    body <- stmt
    add_type (FOR (Just ini) cond inc body) tok
  else if head_equal ts (Keyword "if")
  then do
    tok <- popHeadToken
    skip (Punct "(")
    cond <- expr
    skip (Punct ")")
    _then <- stmt

    elseBlock <- head_equalM (Keyword "else")
    if elseBlock
    then do
      _ <- popHeadToken
      _else <- stmt
      add_type (IF cond _then (Just _else)) tok
    else add_type (IF cond _then Nothing) tok
  else if head_equal ts (Punct "{")
  then do
    _ <- popHeadToken
    compound_stmt
  else expr_stmt
-}


compoundStmt toks  = do
  tok <- lookAhead toks
  (nodes, toks) <- iter toks
  return (Block (mkCtx tok) nodes, toks)

  where
    iter toks = do
      tok <- lookAhead toks
      case tokenKind tok of
        (Punct "}") -> return ([], toks)
        _ ->  do
          res <- isType toks
          if res
          then do
            tbd "iter"
            -- node <- declaration
           -- iter (nodes ++ [node])
          else do
            (node, toks) <- stmt toks
            (nodes,toks) <- iter toks
            return (node : nodes, toks)



function :: [Token] -> Either Error (Decl, [Token])
function toks = do
  (ty, toks) <- declspec toks
  (tok, _) <- headToken toks
  (ftype, toks) <- declarator ty toks

  toks <- skip (Punct "{") toks

  (statements, toks) <- compoundStmt toks

  toks <- skip (Punct "}") toks
  return (mkFuncDecl ty "???" [statements] tok, toks)


  --let nodes = [s]

toplevel :: [Token] -> Either Error (Decl, [Token])
toplevel = parserIF isFunction function globalVariable


program :: [Token] -> Either Error Program
program toks = do
    prog <- iter toks
    return $ Program prog
    where
      iter [] = return []
      iter toks = do
        (decl, toks)  <- toplevel toks
        prog <- iter toks
        return (decl : prog)

removeEOF [] = []
removeEOF (x : xs) = case tokenKind x of
  EOF -> []
  _ -> x : removeEOF xs



parse2 :: [Token] -> Either Error Program
parse2 toks = do program  (removeEOF toks)
