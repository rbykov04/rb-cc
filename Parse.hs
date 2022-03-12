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


add        = do
  node <- mul
  join_ node
  where
    join_ lhs = do
      ts <- getTokens
      if head_equal ts (Punct "+")
      then do
        tok <- popHeadToken
        rhs <- mul
        n_ <- new_add lhs rhs tok
        join_ n_
      else if head_equal ts (Punct "-")
      then do
        tok <- popHeadToken
        rhs <- mul
        n_ <- new_sub lhs rhs tok
        join_ $ n_
      else
        return lhs


 -- join_bin mul        [("+", BIN_OP Add), ("-", BIN_OP Sub)]

assign = do
  lhs@(Node _ lhs_ty _) <- equality
  ts <- getTokens
  if head_equal ts (Punct "=")
  then do
    tok <- popHeadToken
    rhs <- assign
    return $ Node (BIN_OP Assign lhs rhs) lhs_ty tok
  else return lhs


expr       = assign

-- unary = ("+" | "-" | "*" | "&") unary
unary = do
  toks@(t: ts) <- getTokens
  if head_equal toks (Punct "+")
  then do
    _ <- popHeadToken
    unary
  else if head_equal toks (Punct "-")
  then do
      tok <- popHeadToken
      node <- unary
      return $ Node (UNARY Neg node) INT tok
  else if head_equal toks (Punct "&")
  then do
      tok <- popHeadToken
      node@(Node _ ty _) <- unary
      return $ Node (UNARY Addr node) (PTR ty) tok
  else if head_equal toks (Punct "*")
  then do
      tok <- popHeadToken
      node@(Node _ ty _) <- unary
      case ty of
        PTR base -> return $ Node (UNARY Deref node) base tok
        _        -> return $ Node (UNARY Deref node) INT tok
   else primary


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
  (t: ts) <- getTokens
  putTokens ts
  case t of
    (Token (Num v) _ _) -> do
       return $ Node (NUM v) INT t
    (Token (Ident str) _ _) -> do
      isFunc <- head_equalM (Punct "(")
      if isFunc
      then do
        putTokens (t:ts)
        funcall
      else do
        fv <- find_var str
        case fv of
          Nothing -> throwE (ErrorToken t "undefined variable")
          Just var -> return $ Node (VAR var) INT t
    (Token (Punct "(") _ _) -> do
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
  if head_equal (t:ts) tok
  then do
    putTokens ts
    return ()
  else throwE (ErrorToken t ("expected" ++ show tok))

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
    return $ Node (EXPS_STMT [node]) INT tok

-- declspec = "int"
declspec   :: ExceptT Error (State ParserState) Type
declspec = do
  skip (Keyword "int")
  return INT

-- declarator = "*"* ident
declarator :: Type -> ExceptT Error (State ParserState) Obj
declarator basetype = do
  ty <- ptr_wrap basetype
  tok <- popHeadToken
  case tok of
    Token (Ident name) _ _ -> do
      new_lvar name ty
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
      obj@(Obj _ ty _) <- declarator basety
      isAssign <- head_equalM (Punct "=")
      if isAssign
      then do
        tok <- popHeadToken
        let lhs = Node (VAR obj) ty tok
        rhs <- assign
        let node = Node (BIN_OP Assign lhs rhs) ty tok
        let expression  = Node (EXPS_STMT [node]) INT tok
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

program :: ExceptT Error (State ParserState) [Node]
program = do
  skip (Punct "{")
  ts <- compound_stmt
  return [ts]



parseS :: ExceptT Error (State ParserState) Function
parseS = do
  nodes <- program
  ts <- getTokens
  locals <- getLocals

  if not $ head_equal ts EOF
  then throwE (ErrorToken (head ts) "extra token")
  else return (Function nodes locals 208)

parse :: [Token] -> Either Error (Function, [Token])
parse toks = do
  let (r,s') = runState (runExceptT parseS) (toks, [])
  case r of
    Left e -> Left e
    Right out -> return (out, fst s')
