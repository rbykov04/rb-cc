module Parse2 where
--import RBCC
import Error
import Tokenize
import Data.List



data Context = Context Token
                | NullCtx String
  deriving (Show, Eq)

mkCtx :: Token -> Context
mkCtx = Context

mkNullCtx = NullCtx ""
mkNullCtxText s = NullCtx s

data Primary = Number Context Int
  | Var Context String
  deriving (Eq, Show)

  --FIXME nullCtx for Prim??
data Expr = Prim Primary
          | ASSIGN Context Expr Expr
          deriving (Show, Eq)

data Stmt = Return Context Expr
          | Stmt_Expr Context Expr
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


data MetaIR = LeafIR      Context String                        [String]
            | NodeUnIR    Context String MetaIR                 [String]
            | NodeBinIR   Context String MetaIR MetaIR          [String]
            | NodeListIR  Context String [MetaIR]               [String]
            | NodeList1IR Context String MetaIR        [MetaIR] [String]
  deriving (Show, Eq)

getContext :: MetaIR -> Context
getContext  (LeafIR      ctx _ _)     = ctx
getContext  (NodeUnIR    ctx _ _ _)   = ctx
getContext  (NodeBinIR   ctx _ _ _ _) = ctx
getContext  (NodeListIR  ctx _ _ _)   = ctx
getContext  (NodeList1IR ctx _ _ _ _) = ctx

getFst :: MetaIR -> Maybe MetaIR
getFst  (LeafIR {})               = Nothing
getFst  (NodeUnIR _ _ lhs _)      = Just lhs
getFst  (NodeBinIR _ _ lhs _ _)   = Just lhs
getFst  (NodeListIR  {})          = Nothing
getFst  (NodeList1IR _ _ lhs _ _) = Just lhs

getSnd :: MetaIR -> Maybe MetaIR
getSnd  (LeafIR {})               = Nothing
getSnd  (NodeUnIR {})             = Nothing
getSnd  (NodeBinIR _ _ _ rhs _)   = Just rhs
getSnd  (NodeListIR  {})          = Nothing
getSnd  (NodeList1IR {})          = Nothing

getName :: MetaIR -> String
getName  (LeafIR      _ name _)     = name
getName  (NodeUnIR    _ name _ _)   = name
getName  (NodeBinIR   _ name _ _ _) = name
getName  (NodeListIR  _ name _ _)   = name
getName  (NodeList1IR _ name _ _ _) = name

getLoad :: MetaIR -> [String]
getLoad  (LeafIR      _ _ loads)     = loads
getLoad  (NodeUnIR    _ _ _ loads)   = loads
getLoad  (NodeBinIR   _ _ _ _ loads) = loads
getLoad  (NodeListIR  _ _ _ loads)   = loads
getLoad  (NodeList1IR _ _ _ _ loads) = loads

getType :: MetaIR -> String
getType  (LeafIR      _ _ _)     = "LeafIR"
getType  (NodeUnIR    _ _ _ _)   = "NodeUnIR"
getType  (NodeBinIR   _ _ _ _ _) = "NodeBinIR"
getType  (NodeListIR  _ _ _ _)   = "NodeListIR"
getType  (NodeList1IR _ _ _ _ _) = "NodeList1Ir"

getChildren :: MetaIR -> Maybe [MetaIR]
getChildren  (LeafIR {})               = Nothing
getChildren  (NodeUnIR {})             = Nothing
getChildren  (NodeBinIR {})            = Nothing
getChildren  (NodeListIR  _ _ chl _)   = Just chl
getChildren  (NodeList1IR _ _ _ chl _) = Just chl




printLoad' :: Int -> [String] -> [String]
printLoad' _ [] = []
printLoad' tab load =
    ["Loads {"]
    ++ addTab (tab+1) load
    ++ ["}"]

printLoad :: Int -> [String] -> [String]
printLoad _ [] = []
printLoad tab load = ["load = " ++ concat  load]



instance Printable MetaIR where
  pprint :: MetaIR -> [String]
  pprint _ = []

  prettyCodePrint :: Int -> MetaIR -> [String]
  prettyCodePrint tab a =
    let title = [getName a ++ " :: " ++ getType a]
        children = case getChildren a of
                      Nothing -> []
                      Just chl -> concatMap (prettyCodePrint (tab+1)) chl

        lhs = case getFst a of
                  Nothing -> []
                  Just v -> ["lhs = {"]
                              ++ addTab (tab +1) (prettyCodePrint (tab+1) v)
                              ++ ["}"]
        rhs = case getSnd a of
                  Nothing -> []
                  Just v -> ["rhs = {"]
                              ++ addTab (tab +1) (prettyCodePrint (tab+1) v)
                              ++ ["}"]



        body  = printLoad tab (getLoad a) ++ lhs ++ rhs ++ children

    in title ++ addTab (tab+1) body


assertArrEq :: [MetaIR] -> [MetaIR] -> Either (Context, Context) ()
assertArrEq [] [] = Right ()
assertArrEq a [] = Left (getContext (head a), mkNullCtxText "end of Arr")
assertArrEq [] b = Left (mkNullCtxText "End of arr", getContext (head b))
assertArrEq (a:as) (b: bs) = do
  assertEq a b
  assertArrEq as bs



assertEq :: MetaIR -> MetaIR -> Either (Context, Context) ()
assertEq (LeafIR ctxA nameA a) (LeafIR ctxB nameB b) =
  if nameA == nameB && a == b
  then Right ()
  else Left (ctxA, ctxB)
assertEq (NodeUnIR ctxA nameA vA a) (NodeUnIR ctxB nameB vB b) =
  if nameA == nameB && a == b
  then assertEq vA vB
  else Left (ctxA, ctxB)
assertEq (NodeBinIR ctxA nameA lA rA a) (NodeBinIR ctxB nameB lB rB b) =
  if nameA == nameB && a == b
  then do
    assertEq lA lB
    assertEq rA rB
  else Left (ctxA, ctxB)
assertEq (NodeListIR ctxA nameA vA a) (NodeListIR ctxB nameB vB b) =
  if nameA == nameB && a == b
  then do
    assertArrEq vA vB
  else Left (ctxA, ctxB)
assertEq (NodeList1IR ctxA nameA lA vA a) (NodeList1IR ctxB nameB lB vB b) =
  if nameA == nameB && a == b
  then do
    assertEq lA lB
    assertArrEq vA vB
  else Left (ctxA, ctxB)
assertEq a b = Left (getContext a, getContext b)

class Serializable a where
  des :: a -> MetaIR
  ser :: MetaIR -> Either Error a

instance Serializable Primary where
  des (Number ctx v) = LeafIR ctx "Number" [show v]
  des (Var    ctx v) = LeafIR ctx "Var" [v]

instance Serializable Expr where
  des (Prim  v)            = NodeUnIR (mkNullCtxText "Prim") "Prim" (des v) []
  des (ASSIGN ctx lhs rhs) = NodeBinIR ctx "ASSIGN" (des lhs) (des rhs) []

instance Serializable Stmt where
  des (Return ctx v)    = NodeUnIR ctx "Return" (des v) []
  des (Stmt_Expr ctx v) = NodeUnIR ctx "Stmt_Expr" (des v) []
  des (Block ctx arr)   = NodeListIR ctx "Block" (map des arr) []


instance Serializable Declspec where
  des (DeclInt ctx)       = LeafIR    ctx "DeclInt" []
  des (DeclChar ctx)      = LeafIR    ctx "DeclChar" []
  des (DeclArr ty s)      = NodeUnIR mkNullCtx "DeclArr" (des ty) [show s]
  des (DeclPtr v)         = NodeUnIR  mkNullCtx "DeclPtr" (des v) []
  des (DeclFunc v ctx)    = NodeUnIR  ctx "DeclFunc" (des v) []

instance Serializable VarDecl where
  des (Pointer  ctx name ty)       = NodeUnIR ctx "Pointer"  (des ty) [show name]
  des (Variable ctx name ty)       = NodeUnIR ctx "Variable" (des ty) [show name]


instance Serializable Decl where
  des (Function     ctx ty name v)  = NodeList1IR ctx "Function"  (des ty) (map des v) [show name]
  des (VariableDecl ctx ty v)       = NodeList1IR ctx "VariableDecl" (des ty) (map des v) []


instance Serializable Program where
  des (Program v)  = NodeListIR (mkNullCtxText "Program") "Program" (map des v) []



declspecIR toks = do
  (tok , toks) <- headToken toks
  case tokenKind tok of
    (Ident "int") -> return (DeclInt (mkCtx tok), toks)
    _               -> mkTokError tok "Unkown type"

getIdent toks = do
  (tok , toks) <- headToken toks
  case tokenKind tok of
   (Ident x)  -> return (x, toks)
   _ -> mkTokError tok "Expected ident"



varDecl toks = do
  tok <- lookAhead toks
  (name , toks) <- getIdent toks
  toks <- skip (Punct ":") toks
  (ty, toks) <- declspecIR toks
  return (Variable (mkCtx tok) name ty, toks)


topLevelIR :: [Token] -> Either Error (Decl, [Token])
topLevelIR toks = do
  (tok , toks) <- headToken toks
  case tokenKind tok of
   (Ident "vars")  -> do
      (ty, toks) <- declspecIR toks
      toks <- skip (Punct "{") toks
      (vars , toks) <- iter toks
      toks <- skip (Punct "}") toks
      return (VariableDecl (mkCtx tok) ty vars, toks)

      where iter toks = do
             tok <- lookAhead toks
             case tokenKind tok of
               (Punct "}") -> return ([], toks)
               _ -> do
                 (var, toks) <- varDecl toks
                 (vars, toks) <- iter toks
                 return (var : vars, toks)


   (Ident "fn")    -> tbd $ "fn" ++ show tok
   _               -> mkTokError tok "Expected or vars or fn"

programIR :: [Token] -> Either Error Program
programIR toks = do
    prog <- iter toks
    return $ Program prog
    where
      iter [] = return []
      iter toks = do
        (decl, toks)  <- topLevelIR toks
        prog <- iter toks
        return (decl : prog)


parseIR :: [Token] -> Either Error Program
parseIR toks = do programIR  (removeEOF toks)

--parseIR tok = return Program []



addTab :: Int -> [String] -> [String]
addTab tab arr =
  let
      prefix = replicate (tab*4) ' '
  in map (\x -> prefix ++ x) arr

class Printable a where
  pprint :: a -> [String]
  pprintOneLine :: a -> String
  pprintOneLine a = concat (pprint a)
  prettyCodePrint :: Int -> a -> [String]
  prettyCodePrint tab a =
    let
      code = pprint a
    in addTab tab code



instance Printable Primary where
  pprint :: Primary -> [String]
  pprint (Number _ v) = ["number " ++ show v]
  pprint (Var _ v) = ["var " ++ v]

instance Printable Expr where
  pprint :: Expr -> [String]
  pprint (Prim p) = pprint p
  pprint (ASSIGN _ lhs rhs) = ["assign (" ++ pprintOneLine lhs ++ ") = (" ++ pprintOneLine rhs ++ ")"]
  pprint _ = []



instance Printable Stmt where
  pprint :: Stmt -> [String]
  pprint (Return _ ex)    = ["return " ++ pprintOneLine ex]
  pprint (Stmt_Expr _ ex) = ["stmt : " ++ pprintOneLine ex]
  pprint (Block _ body)   =
    ["block {"]
    ++ concatMap pprint body
    ++ ["}"]
  pprint _                = []

  prettyCodePrint :: Int -> Stmt -> [String]
  prettyCodePrint tab (Block _ body) =
      let code = ["block {"] ++ concatMap (prettyCodePrint (tab+1)) body ++ ["}"]
      in addTab tab code
  prettyCodePrint tab st =
    let code = pprint st
    in addTab tab code



instance Printable Declspec where
  pprint :: Declspec -> [String]
  pprint (DeclInt _)= ["int"]
  pprint _ = []

instance Printable VarDecl where
  pprint :: VarDecl -> [String]
  pprint (Variable _ name ty) = [name ++ " : " ++ pprintOneLine ty]
  pprint (Pointer _ name ty)  = [name ++ " : " ++ pprintOneLine ty]



instance Printable Decl where
  pprint :: Decl -> [String]
  pprint (Function _ ty name body ) =
    ["fn " ++ name]
    ++ ["args {"]
    ++ ["}"]
    ++ ["rettype " ++ pprintOneLine ty]
    ++ ["body {"]
    ++ concatMap pprint body
    ++ ["}"]
  pprint (VariableDecl _ ty vars) =
    ["vars " ++ pprintOneLine ty ++"{"]
    ++ concatMap pprint vars
    ++ ["}"]

  prettyCodePrint :: Int -> Decl -> [String]
  prettyCodePrint tab (Function _ ty name body ) =
    ["fn " ++ name]
    ++ ["args {"]
    ++ ["}"]
    ++ ["rettype " ++ pprintOneLine ty]
    ++ ["body {"]
    ++ concatMap (prettyCodePrint (tab+1)) body
    ++ ["}"]
  prettyCodePrint tab (VariableDecl _ ty vars) =
    ["vars " ++ pprintOneLine ty ++" {"]
    ++ concatMap (prettyCodePrint (tab+1)) vars
    ++ ["}"]

instance Printable Program where
  pprint :: Program -> [String]
  pprint (Program prog) = concatMap pprint prog

  prettyCodePrint :: Int -> Program -> [String]
  prettyCodePrint tab (Program prog) = concatMap (prettyCodePrint tab) prog



printProgram :: Program -> [String]
printProgram = prettyCodePrint 0

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
    Ident name -> do
      return (Var (mkCtx tok) name, toks)
    _ -> tbd $ "primary " ++ show tok


equality  toks     = do
  (p, toks) <- primary toks
  return (Prim p, toks)

assign toks = do
  (lhs, toks) <- equality toks
  tok <- lookAhead toks
  case tokenKind tok of
    (Punct "=") -> do
      toks <- skip (Punct "=") toks
      (rhs, toks) <- assign toks
      return (ASSIGN (mkCtx tok) lhs rhs, toks)
    _ -> return (lhs, toks)

expr       = assign

-- expr-stmt = expr? ";"
expr_stmt toks = do
  tok <- lookAhead toks
  case tokenKind tok of
    -- FIXME it is a rubbish: why can't we skip this?
    (Punct ";") -> do
      toks <- skip (Punct ";") toks
      return (Block (mkCtx tok) [], toks)
    _ -> do
      (node, toks) <- expr toks
      toks <- skip (Punct ";") toks
      return (Stmt_Expr (mkCtx tok) node, toks)



--stmt = "return" expr ";"
--     | "if" "(" expr ")" stmt ("else" stmt)?
--     | "for" "(" expr-stmt expr? ";" expr? ")" stmt
--     | "while" "(" expr" )" stmt
--     | "{" compound-stmt
--     | expr-stmt

stmt toks = do
  (tok, toks_) <- headToken toks
  case tokenKind tok of
    (Keyword "return") -> do
      (node, toks) <- expr toks_
      toks <- skip (Punct ";") toks
      return (Return (mkCtx tok) node, toks)
    _ -> expr_stmt toks
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
  case tokenKind tok of
    (Ident name) -> do
      (ftype, toks) <- declarator ty toks
      toks <- skip (Punct "{") toks
      (statements, toks) <- compoundStmt toks
      toks <- skip (Punct "}") toks
      return (mkFuncDecl ty name [statements] tok, toks)
    _ -> mkTokError tok "Expected Ident"


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

