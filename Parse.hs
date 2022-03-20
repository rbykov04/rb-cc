-- This file contains a recursive descent parser for C.
--
-- Most functions in this file are named after the symbols they are
-- supposed to read from an input token list. For example, stmt() is
-- responsible for reading a statement from a token list. The function
-- then construct an AST node representing a statement.
--
-- Each function conceptually returns two values, an AST node and
-- remaining part of the input tokens. Since C doesn't support
-- multiple return values, the remaining tokens are returned to the
-- caller via a pointer argument.
--
-- Input tokens are represented by a linked list. Unlike many recursive
-- descent parsers, we don't have the notion of the "input token stream".
-- Most parsing functions don't change the global state of the parser.
-- So it is very easy to lookahead arbitrary number of tokens in this
-- parser.

module Parse where
import Control.Monad.Trans.Except
import Control.Monad.State
import System.Environment
import System.IO
import RBCC
import Codegen
import Tokenize
import Type
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
          node <- add_type (op lhs rhs) tok
          join_ node
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

make_offset ty count tok = do
  base <- add_type (NUM ((typeSize) ty)) tok
  add_type (BIN_OP Mul count base) tok

new_add  :: Node->Node->Token -> ExceptT Error (State ParserState) Node
new_add lhs rhs tok = case ((typeKind . nodeType) lhs, (typeKind . nodeType) rhs) of
  (INT, INT) -> do
    add_type (BIN_OP Add lhs rhs) tok

  ((PTR ty), INT)     -> ptr_math lhs ty rhs tok
  ((ARRAY ty _), INT) -> ptr_math lhs ty rhs tok

  (INT, (ARRAY ty _)) -> ptr_math rhs ty lhs tok
  (INT, (PTR ty))     -> ptr_math rhs ty lhs tok

  -- ptr +  ptr
  _ ->  throwE (ErrorToken tok "invalid operands")
  where
    ptr_math ptr ty count token = do
      offset <- make_offset ty count token
      add_type (BIN_OP Add ptr offset) token


new_sub lhs rhs tok = case ((typeKind . nodeType) lhs, (typeKind . nodeType) rhs) of
  (INT, INT) -> do
    add_type (BIN_OP Sub lhs rhs) tok

  ((PTR ty), INT)     -> ptr_math lhs ty rhs tok
  ((ARRAY ty _), INT) -> ptr_math lhs ty rhs tok

-- ptr - ptr, which returns how many elements are between the two.
  ((PTR tyBase), (PTR _))         -> ptr_diff lhs tyBase rhs tok
  ((PTR tyBase), (ARRAY _ _))     -> ptr_diff lhs tyBase rhs tok
  ((ARRAY tyBase _), (PTR _))     -> ptr_diff lhs tyBase rhs tok
  ((ARRAY tyBase _), (ARRAY _ _)) -> ptr_diff lhs tyBase rhs tok

-- num - ptr
  _ -> throwE (ErrorToken tok "invalid operands")
  where
    ptr_diff a atype b token = do
      base <- add_type (NUM (typeSize atype)) token
      num <- add_type (BIN_OP Sub a b) tok
      add_type (BIN_OP Div (change_type make_int num) base) tok

    ptr_math ptr ty count token = do
      offset <- make_offset ty count token
      add_type (BIN_OP Sub ptr offset) token

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
  lhs <- equality
  kind <- seeHeadTokenKind
  case kind of
    Punct "=" -> do
      tok <- popHeadToken
      rhs <- assign
      add_type (Assign lhs rhs) tok
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
      add_type (UNARY Neg node) tok
    Punct "&" -> do
      tok <- popHeadToken
      node <- unary
      add_type (UNARY Addr node) tok
    Punct "*" -> do
      tok <- popHeadToken
      node <- unary
      add_type (UNARY Deref node) tok

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
      add_type (FUNCALL name args) ident
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
       add_type (NUM v) t
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
            Just var -> add_type (VAR var) t

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
    add_type (BLOCK []) tok
  else do
    tok <- seeHeadToken
    node <- expr
    skip (Punct ";")
    add_type (EXPS_STMT node) tok

-- declspec = "int"
declspec   :: ExceptT Error (State ParserState) Type
declspec = do
  skip (Keyword "int")
  return make_int

-- func-params = param ("," param)*
-- param       = declspec declarator
func_params :: Type -> ExceptT Error (State ParserState) Type
func_params base = do
  args <- iter []
  skip (Punct ")")
  return $ func_type base args
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

getNumber :: ExceptT Error (State ParserState) Int
getNumber = do
   tok <- popHeadToken
   case tokenKind tok of
      Num v -> return v
      _ -> throwE (ErrorToken tok "expected an number")


-- type-suffix = (" func-params
--             | "[" num "]"
--             | etc
type_suffix :: Type -> ExceptT Error (State ParserState) Type
type_suffix base = do
  kind <- seeHeadTokenKind
  case kind of
    Punct "(" -> do
      skip (Punct "(")
      func_params base
    Punct "[" -> do
      skip (Punct "[")
      len <- getNumber
      skip (Punct "]")
      return $ array_of base len
    _ -> return base

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
      then ptr_wrap $ pointer_to base
      else return base

-- declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
declaration = do
  tok <- seeHeadToken
  basety <- declspec
  nodes <- iter basety []
  skip (Punct ";")
  add_type (BLOCK nodes) tok
  where
    decl_expr basety nodes = do
      (ty, name) <- declarator basety
      obj <- new_lvar name ty
      isAssign <- head_equalM (Punct "=")
      if isAssign
      then do
        tok <- popHeadToken
        lhs <- add_type (VAR obj) tok
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

compound_stmt  = do
  tok <- seeHeadToken
  nodes <- iter []

  add_type (BLOCK nodes) tok
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
    add_type (RETURN node) tok
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


create_param_lvars :: [(Type,String)] -> ExceptT Error (State ParserState) [Obj]
create_param_lvars = mapM $ (uncurry . flip) new_lvar

function :: ExceptT Error (State ParserState) Function
function = do
  t <- seeHeadToken
  putLocals []
  ty <- declspec
  (ftype, name) <- declarator ty
  args <- create_args $ typeKind ftype
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
