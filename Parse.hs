module Parse where
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Environment
import System.IO
import RBCC
import Codegen
import Tokenize
import Data.List

head_equal :: [Token] -> TokenKind -> Bool
head_equal ((Token (Punct a) _ _) : _) (Punct b) = a == b
head_equal ((Token (Ident a) _ _) : _) (Ident b) = a == b
head_equal ((Token (Keyword a) _ _) : _) (Keyword b) = a == b
head_equal ((Token EOF _ _) : _) EOF = True
head_equal [] _ = False
head_equal _ (Num _) = False
head_equal _ (Punct _) = False
head_equal _ (Keyword _) = False
head_equal _ _ = False


head_equalM :: TokenKind -> ExceptT Error (State ParserState) Bool
head_equalM token = do
  ts <- getTokens
  return $ head_equal ts token

popHeadToken :: ExceptT Error (State ParserState) Token
popHeadToken = do
  (h:ts) <- getTokens
  putTokens ts
  return h

seeHeadToken :: ExceptT Error (State ParserState) Token
seeHeadToken = do
  (h:_) <- getTokens
  return h

seeHeadTokenKind :: ExceptT Error (State ParserState) TokenKind
seeHeadTokenKind = do
  tok <- seeHeadToken
  return $ tokenKind tok

type ParserState = ([Token], [Obj])

stmt          :: ExceptT Error (State ParserState) Node
expr_stmt     :: ExceptT Error (State ParserState) Node
primary       :: ExceptT Error (State ParserState) Node
funcall       :: ExceptT Error (State ParserState) Node
unary         :: ExceptT Error (State ParserState) Node
assign        :: ExceptT Error (State ParserState) Node
compound_stmt :: ExceptT Error (State ParserState) Node
declaration   :: ExceptT Error (State ParserState) Node
expr          :: ExceptT Error (State ParserState) Node

equality      :: ExceptT Error (State ParserState) Node
relational    :: ExceptT Error (State ParserState) Node
add           :: ExceptT Error (State ParserState) Node
mul           :: ExceptT Error (State ParserState) Node



getTokens :: ExceptT Error (State ParserState) [Token]
getTokens = do
  r <- get
  return $ fst r

putTokens :: [Token] -> ExceptT Error (State ParserState) ()
putTokens toks = do
  r <- get
  put (toks, snd r)


getLocals :: ExceptT Error (State ParserState) [Obj]
getLocals = do
  r <- get
  return $ snd r

putLocals :: [Obj] -> ExceptT Error (State ParserState) ()
putLocals vars = do
  r <- get
  put (fst r, vars)

join_bin ::
   ExceptT Error (State ParserState) Node
   -> [(String, (Node -> Node -> Node_))]
   -> ExceptT Error (State ParserState) Node

join_bin sub bin_ops = do
  node <- sub
  join_ node
  where
    join_ lhs = do
      toks <- getTokens
      let tokKind = (tokenKind . head) toks
      case lookup tokKind (map toPunct bin_ops) of
        Just op -> do
          tok <- popHeadToken
          rhs <- sub
          join_ (Node (op lhs rhs) INT tok)
        Nothing ->  return lhs


toPunct (str, op) = (Punct str, op)

equality   = join_bin relational [("==", BIN_OP ND_EQ), ("!=", BIN_OP ND_NE)]
relational = join_bin add
  [
    ("<",  BIN_OP ND_LT),
    ("<=", BIN_OP ND_LE),
    (">",  flip (BIN_OP ND_LT)),
    (">=", flip (BIN_OP ND_LE))
  ]

mul        = join_bin unary      [("*", BIN_OP Mul), ("/", BIN_OP Div)]

-- In C, `+` operator is overloaded to perform the pointer arithmetic.
-- If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
-- so that p+n points to the location n elements (not bytes) ahead of p.
-- In other words, we need to scale an integer value before adding to a
-- pointer value. This function takes care of the scaling.

make_offset count tok =  offset where
  base = Node (NUM 8) INT tok
  offset = Node (BIN_OP Mul count base) INT tok

new_add  :: Node->Node->Token -> ExceptT Error (State ParserState) Node
new_add lhs@(Node _ INT _) rhs@(Node _ INT _) tok = do
  return $ Node (BIN_OP Add lhs rhs) INT tok

new_add lhs@(Node _ (PTR base) _) rhs@(Node _ INT _) tok = do
  let offset = make_offset rhs tok
  return $ Node (BIN_OP Add lhs offset) (PTR base) tok

new_add lhs@(Node _ INT _) rhs@(Node _ (PTR base) _) tok = do
  let offset = make_offset lhs tok
  return $ Node (BIN_OP Add rhs offset) (PTR base) tok

-- ptr +  ptr
new_add _ _ t = throwE (ErrorToken t "invalid operands")

new_sub  :: Node->Node->Token -> ExceptT Error (State ParserState) Node
new_sub lhs@(Node _ INT _) rhs@(Node _ INT _) tok = do
  return $ Node (BIN_OP Sub lhs rhs) INT tok

-- ptr - num
new_sub ptr@(Node _ (PTR base) _) count@(Node _ INT _) tok = do
  let offset = make_offset count tok
  return $ Node (BIN_OP Sub ptr offset) (PTR base) tok

-- ptr - ptr, which returns how many elements are between the two.
new_sub lhs@(Node _ (PTR _) _) rhs@(Node _ (PTR _) _) tok = do
  let base = Node (NUM 8) INT tok
  let num = Node (BIN_OP Sub lhs rhs) INT tok
  return $ Node (BIN_OP Div num base) INT tok

-- num - ptr
new_sub _ _ t = throwE (ErrorToken t "invalid operands")

add  = do
  node <- mul
  join_ node
  where
    join_ lhs = do
      kind <- seeHeadTokenKind
      case kind of
        Punct "+" -> do
          tok <- popHeadToken
          rhs <- mul
          n_ <- new_add lhs rhs tok
          join_ n_
        Punct "-" -> do
          tok <- popHeadToken
          rhs <- mul
          n_ <- new_sub lhs rhs tok
          join_ $ n_
        _ -> return lhs

 -- join_bin mul        [("+", BIN_OP Add), ("-", BIN_OP Sub)]

assign = do
  lhs@(Node _ lhs_ty _) <- equality
  kind <- seeHeadTokenKind
  case kind of
    Punct "=" -> do
      tok <- popHeadToken
      rhs <- assign
      return $ Node (Assign lhs rhs) lhs_ty tok
    _ -> return lhs

expr       = assign

-- unary = ("+" | "-" | "*" | "&") unary
unary = do
  kind <- seeHeadTokenKind
  case kind of
    Punct "+" -> do
      _ <- popHeadToken
      unary
    Punct "-" -> do
      tok <- popHeadToken
      node <- unary
      return $ Node (UNARY Neg node) INT tok
    Punct "&" -> do
      tok <- popHeadToken
      node@(Node _ ty _) <- unary
      return $ Node (UNARY Addr node) (PTR ty) tok
    Punct "*" -> do
      tok <- popHeadToken
      node@(Node _ ty _) <- unary
      case ty of
        PTR base -> return $ Node (UNARY Deref node) base tok
        _        -> return $ Node (UNARY Deref node) INT tok
    _ -> primary


find_var :: String -> ExceptT Error (State ParserState) (Maybe Obj)
find_var var = do
  vars <- getLocals
  return (find f vars) where
    f (Obj name _ _) = var == name

new_lvar :: String -> Type -> ExceptT Error (State ParserState) Obj
new_lvar name t = do
  vars <- getLocals
  let v = Obj name t 0
  putLocals (v:vars)
  return v

-- funcall == ident "(" (assign ("," assign)*)? ")"
funcall = do
  ident <- popHeadToken
  case ident of
    (Token (Ident name) _ _) -> do
      skip (Punct "(")
      args <- get_args
      skip (Punct ")")
      return $ Node (FUNCALL name args) INT ident
    _ -> throwE (ErrorToken ident "expected an ident")
  where
    get_args = do
      isEnd <- head_equalM (Punct ")")
      if isEnd
      then return []
      else do
        arg <- assign
        f_iter [arg]

    f_iter args = do
      isEnd <- head_equalM (Punct ")")
      if isEnd
      then return args
      else do
        skip (Punct ",")
        arg <- assign
        f_iter $ args ++ [arg]


-- primary = "(" expr ")" | ident args? | num
-- args = "(" ")"
primary = do
  (t@(Token kind _ _): ts) <- getTokens
  putTokens ts

  case kind of
    Num v -> do
       return $ Node (NUM v) INT t
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
            Just var -> return $ Node (VAR var) INT t
    Punct "(" -> do
      node <- expr
      skip (Punct ")")
      return node
    _ -> throwE (ErrorToken t "expected an expression")

consume :: TokenKind -> ExceptT Error (State ParserState) Bool
consume tok = do
  (t:ts) <- getTokens
  if head_equal (t:ts) tok
  then do
    putTokens ts
    return True
  else return False

skip :: TokenKind -> ExceptT Error (State ParserState) ()
skip tok = do
  (t:ts) <- getTokens
  unless (head_equal (t:ts) tok) $ throwE (ErrorToken t ("expected "  ++ show tok))
  putTokens ts

-- expr-stmt = expr? ";"
expr_stmt = do
  emptyBlock <- head_equalM (Punct ";")
  if emptyBlock
  then do
    tok <- popHeadToken
    return $ Node (BLOCK []) INT tok
  else do
    tok <- seeHeadToken
    node <- expr
    skip (Punct ";")
    return $ Node (EXPS_STMT node) INT tok

-- declspec = "int"
declspec   :: ExceptT Error (State ParserState) Type
declspec = do
  skip (Keyword "int")
  return INT

-- type-suffix = ("(" func-params? ")")?
-- func-params = param ("," param)*
-- param       = declspec declarator
type_suffix :: Type -> ExceptT Error (State ParserState) Type
type_suffix base = do
  isFunc <- head_equalM (Punct "(")
  if isFunc
  then do
    skip (Punct "(")
    args <- iter []
    skip (Punct ")")
    return $ FUNC base args
  else return base
  where
    iter params = do
      isEnd <- head_equalM (Punct ")")
      if isEnd then return params
      else do
         basety <- declspec
         arg <- declarator basety
         isNext <- head_equalM (Punct ",")
         let res = params ++ [arg]
         if isNext
         then do
           skip (Punct ",")
           iter res
         else return res


-- declarator = "*"* ident type-suffix
declarator :: Type -> ExceptT Error (State ParserState) (Type, String)
declarator basetype = do
  ty <- ptr_wrap basetype
  tok <- popHeadToken
  case tok of
    Token (Ident name) _ _ -> do
      decl_type <- type_suffix ty
      return (decl_type, name)
    _ -> throwE (ErrorToken tok "expected an identifier")
  where
    ptr_wrap base = do
      isPtr <- consume (Punct "*")
      if isPtr
      then ptr_wrap (PTR base)
      else return base



-- declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
declaration = do
  tok <- seeHeadToken
  basety <- declspec
  nodes <- iter basety []
  skip (Punct ";")
  return $ Node (BLOCK nodes) basety tok
  where
    decl_expr basety nodes = do
      (ty, name) <- declarator basety
      obj <- new_lvar name ty
      isAssign <- head_equalM (Punct "=")
      if isAssign
      then do
        tok <- popHeadToken
        let lhs = Node (VAR obj) ty tok
        rhs <- assign
        let node = Node (Assign lhs rhs) ty tok
        let expression  = Node (EXPS_STMT node) INT tok
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

compound_stmt  = do
  tok <- seeHeadToken
  nodes <- iter []
  return $ Node (BLOCK nodes) INT tok
  where
    iter nodes = do
      endBlock <- head_equalM (Punct "}")
      if endBlock
      then do
        _ <- popHeadToken
        return nodes
      else do
        ts <- getTokens
        if head_equal ts (Keyword "int")
        then do
          node <- declaration
          iter (nodes ++ [node])
        else do
          node <- stmt
          iter (nodes ++ [node])

maybe_expr :: TokenKind -> ExceptT Error (State ParserState) (Maybe Node)
maybe_expr tk = do
  isNotExpr <- head_equalM tk
  if isNotExpr
  then return Nothing
  else do
    node <- expr
    return $ Just node

--stmt = "return" expr ";"
--     | "if" "(" expr ")" stmt ("else" stmt)?
--     | "for" "(" expr-stmt expr? ";" expr? ")" stmt
--     | "while" "(" expr" )" stmt
--     | "{" compound-stmt
--     | expr-stmt
stmt  = do
  ts <- getTokens
  if head_equal ts (Keyword "return")
  then do
    tok <- popHeadToken
    node <-expr
    skip (Punct ";")
    return $ Node (RETURN node) INT tok
  else if head_equal ts (Keyword "while")
  then do
    tok <- popHeadToken
    skip (Punct "(")
    cond <- expr
    skip (Punct ")")
    body <- stmt
    return $ Node (FOR Nothing (Just cond) Nothing body) INT tok
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
    return $ Node (FOR (Just ini) cond inc body) INT tok
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
      return $ Node (IF cond _then (Just _else)) INT tok
    else return $ Node (IF cond _then Nothing) INT tok
  else if head_equal ts (Punct "{")
  then do
    _ <- popHeadToken
    compound_stmt
  else expr_stmt

create_param_lvars :: [(Type,String)] -> ExceptT Error (State ParserState) [Obj]
create_param_lvars = mapM $ (uncurry . flip) new_lvar

function :: ExceptT Error (State ParserState) Function
function = do
  t <- seeHeadToken
  putLocals []
  ty <- declspec
  (ftype, name) <- declarator ty
  args <- create_args ftype
  skip (Punct "{")
  s <- compound_stmt
  let nodes = [s]

  locals <- getLocals
  return $ Function nodes locals 208 name args ftype
  where
    create_args ty =
      case ty of
        FUNC _ args -> create_param_lvars args
        _ -> do
          t <- seeHeadToken
          throwE (ErrorToken t "incorrect type for func")

-- program = function-definition*
program :: ExceptT Error (State ParserState) [Function]
program = iter []
  where
    iter funcs = do
      isEnd <- head_equalM EOF
      if isEnd
      then return funcs
      else do
        f <- function
        iter $ funcs ++ [f]

parse :: [Token] -> Either Error ([Function], [Token])
parse toks = do
  let (r,s') = runState (runExceptT program) (toks, [])
  case r of
    Left e -> Left e
    Right out -> return (out, fst s')
