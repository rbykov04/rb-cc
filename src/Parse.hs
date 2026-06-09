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
import System.Environment
import System.IO
import AST
import Codegen
import Tokenize
import Semantic
import Scopechecker
import Error
import Data.List

import Control.Monad.Trans.Except
import Control.Monad.State

import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap


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
  toks <- getTokens
  case toks of
    (h:ts) -> do
      putTokens ts
      return h
    [] -> error "not achivable"

seeHeadToken :: ExceptT Error (State ParserState) Token
seeHeadToken = do
  toks <- getTokens
  case toks of
    (h:_) -> do
      return h
    [] -> error "not achivable"


seeHeadTokenKind :: ExceptT Error (State ParserState) TokenKind
seeHeadTokenKind = do
  tok <- seeHeadToken
  return $ tokenKind tok



type Parser = ExceptT Error (State ParserState) (Node Typed)
stmt          :: Parser
expr_stmt     :: Parser
primary       :: Parser
funcall       :: Parser
unary         :: Parser
assign        :: Parser
compound_stmt :: Parser
declaration   :: Parser
expr          :: Parser

equality      :: Parser
relational    :: Parser
add           :: Parser
mul           :: Parser
postfix       :: Parser

getTokens :: ExceptT Error (State ParserState) [Token]
getTokens = do
  r <- get
  return $ tokens r

putTokens :: [Token] -> ExceptT Error (State ParserState) ()
putTokens toks = do
  r <- get
  put r {tokens = toks}

join_bin ::
   ExceptT Error (State ParserState) (Node Typed)
   -> [(String, Node Typed ->Node Typed->Token -> ExceptT Error (State ParserState) (Node Typed))]
   -> ExceptT Error (State ParserState) (Node Typed)

join_bin sub bin_ops = do
  node <- sub
  join_ node
  where
    join_ lhs = do
      toks <- getTokens
      let tokKind = (tokenKind . head) toks
      case lookup tokKind (map toPunct bin_ops) of
        Just make_node -> do
          tok <- popHeadToken
          rhs <- sub
          node <-  make_node lhs rhs tok
          join_ node
        Nothing ->  return lhs

toPunct (str, op) = (Punct str, op)

add_untyped_node node tok = do
  return $ Node node (tok,  make_untyped)

add_bin op lhs rhs tok = add_untyped_node (BIN_OP op lhs rhs) tok

mul        = join_bin unary      [("*", add_bin Mul), ("/", add_bin Div)]
add        = join_bin mul        [("+", add_bin Add), ("-", add_bin Sub)]
equality   = join_bin relational [("==", add_bin ND_EQ), ("!=", add_bin ND_NE)]
relational = join_bin add
  [
    ("<",  add_bin ND_LT),
    ("<=", add_bin ND_LE),
    (">",  flip (add_bin ND_LT)),
    (">=", flip (add_bin ND_LE))
  ]

-- In C, `+` operator is overloaded to perform the pointer arithmetic.
-- If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
-- so that p+n points to the location n elements (not bytes) ahead of p.
-- In other words, we need to scale an integer value before adding to a
-- pointer value. This function takes care of the scaling.

assign = do
  lhs <- equality
  kind <- seeHeadTokenKind
  case kind of
    Punct "=" -> do
      tok <- popHeadToken
      rhs <- assign
      add_untyped_node (Assign lhs rhs) tok
    _ -> return lhs

expr       = assign

-- unary = ("+" | "-" | "*" | "&") unary
--         | postfix
unary = do
  kind <- seeHeadTokenKind
  case kind of
    Punct "+" -> popHeadToken >> unary
    Punct "-" -> add_unary_type Neg
    Punct "&" -> add_unary_type Addr
    Punct "*" -> add_unary_type Deref
    _ -> postfix
    where
      add_unary_type op = do
        tok <- popHeadToken
        node <- unary
        add_untyped_node (UNARY op node) tok

-- funcall == ident "(" (assign ("," assign)*)? ")"
funcall = do
  ident <- popHeadToken
  case ident of
    (Token (Ident name) _ _) -> do
      skip (Punct "(")
      args <- get_args
      skip (Punct ")")
      add_untyped_node (FUNCALL name args) ident
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


-- primary = "(" "{" stmt+ "}" ")"
--         | "(" expr ")"
--         | "sizeof" unary
--         | ident func-args?
--         | str
--         | num
-- args = "(" ")"
primary = do
  toks <- getTokens
  case toks of
    (t@(Token kind _ _): ts) -> do
      putTokens ts

      case kind of
        Num v -> do
          add_untyped_node (NUM v) t
        Str str -> do
          let ty = array_of make_char (length str + 1)
          o <- new_string_literal (str ++ "\0") ty
          add_untyped_node (VAR o) t
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
                Just var -> add_untyped_node (VAR (objKey var)) t
        Punct "(" -> do
          isGnuStatementExpression <- head_equalM (Punct "{")
          if isGnuStatementExpression
          then do
          -- This is a GNU statement expresssion.
            skip (Punct "{")
            body <- compound_stmt
            skip (Punct ")")
            node <- add_untyped_node (STMT_EXPR body) t
            return node
          else do
            node <- expr
            skip (Punct ")")
            return node
        Keyword "sizeof" -> do
          node <- unary
          add_untyped_node (SIZEOF node) t
        _ -> throwE (ErrorToken t "expected an expression")
    [] -> error "not achivable"

consume :: TokenKind -> ExceptT Error (State ParserState) Bool
consume tok = do
  toks <- getTokens
  case toks of
    (t:ts) -> if head_equal (t:ts) tok
      then do
        putTokens ts
        return True
      else return False
    [] -> error "not achivable"



skip :: TokenKind -> ExceptT Error (State ParserState) ()
skip tok = do
  toks <- getTokens
  case toks of
    (t:ts) -> do
      unless (head_equal (t:ts) tok) $ throwE (ErrorToken t ("expected "  ++ show tok))
      putTokens ts
    [] -> error "not achivable"



-- expr-stmt = expr? ";"
expr_stmt = do
  emptyBlock <- head_equalM (Punct ";")
  if emptyBlock
  then do
    tok <- popHeadToken
    add_untyped_node (BLOCK []) tok
  else do
    tok <- seeHeadToken
    node <- expr
    skip (Punct ";")
    add_untyped_node (EXPS_STMT node) tok

-- declspec = "int" | "char"
declspec   :: ExceptT Error (State ParserState) Type
declspec = do
  kind <- seeHeadTokenKind
  case kind of
    Keyword "char" -> skip (Keyword "char") >> return make_char
    _              -> skip (Keyword "int")  >> return make_int



-- func-params = param ("," param)*
-- param       = declspec declarator
func_params :: Type -> ExceptT Error (State ParserState) Type
func_params base = do
  args <- iter []
  skip (Punct ")")
  args' <- create_param_lvars args
  return $ func_type base args'
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
--             | "[" num "]" type-suffix
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
      node <- type_suffix base
      return $ array_of node len
    _ -> return base

-- postfix = primary ("[" expr "]")
postfix = do
  node <- primary
  iter node
  where
    iter node = do
      kind <- seeHeadTokenKind
      case kind of
        Punct "[" -> do
          -- x[y] is short for *(x+y)
          tok <- popHeadToken
          idx <- expr
          ptr <- add_untyped_node (BIN_OP Add node idx) tok
          node' <- add_untyped_node (UNARY Deref ptr) tok
          skip (Punct "]")
          iter node'
        _ -> return node

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
  add_untyped_node (BLOCK nodes) tok
  where
    decl_expr basety nodes = do
      (ty, name) <- declarator basety
      key <- new_lvar name ty
      isAssign <- head_equalM (Punct "=")
      if isAssign
      then do
        tok <- popHeadToken
        lhs <- add_untyped_node (VAR key) tok
        rhs <- assign
        node <- add_untyped_node (Assign lhs rhs) tok
        expression  <- add_untyped_node (EXPS_STMT node) tok
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

is_type = do
  kind <- seeHeadTokenKind
  case kind of
    (Keyword "int")  -> return True
    (Keyword "char") -> return True
    _                -> return False
compound_stmt  = do
  tok <- seeHeadToken
  enterScope
  nodes <- iter []
  leaveScope

  r <- add_untyped_node (BLOCK nodes) tok
  return r

  where
    iter nodes = do
      endBlock <- head_equalM (Punct "}")
      if endBlock
      then do
        _ <- popHeadToken
        return nodes
      else do
        isType <- is_type
        if isType
        then do
          node <- declaration
          iter (nodes ++ [node])
        else do
          node <- stmt
          iter (nodes ++ [node])

maybe_expr :: TokenKind -> ExceptT Error (State ParserState) (Maybe (Node Typed))
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
    add_untyped_node (RETURN node) tok
  else if head_equal ts (Keyword "while")
  then do
    tok <- popHeadToken
    skip (Punct "(")
    cond <- expr
    skip (Punct ")")
    body <- stmt
    add_untyped_node (FOR Nothing (Just cond) Nothing body) tok
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
    add_untyped_node (FOR (Just ini) cond inc body) tok
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
      add_untyped_node (IF cond _then (Just _else)) tok
    else add_untyped_node (IF cond _then Nothing) tok
  else if head_equal ts (Punct "{")
  then do
    _ <- popHeadToken
    compound_stmt
  else expr_stmt


function :: ExceptT Error (State ParserState) ()
function = do
  putLocals []
  ty <- declspec
  (ftype, name) <- declarator ty
  skip (Punct "{")

  enterScope

  s <- compound_stmt
  let nodes = [s]

  locals <- getLocals
  key <- new_gvar name ftype
  update_var (updateFunc (map objKey locals) nodes) key

  leaveScope
  where
    updateFunc locals nodes obj = obj {objLocals = locals, objBody = nodes};

isFunction :: ExceptT Error (State ParserState) Bool
isFunction = do
  tok <- seeHeadTokenKind
  case tok of
    Punct ";" -> return False
    _ -> do
      _ <- popHeadToken
      (ftype, _) <- declarator make_int
      case typeKind ftype of
        FUNC _ _ _ _ -> return True
        _            -> return False


-- program = function-definition*
program :: ExceptT Error (State ParserState) ()
program = do
  enterScope
  isEnd <- head_equalM EOF
  if isEnd
  then return ()
  else do
    toks <- getTokens
    isFunc <- isFunction
    putTokens toks
    if isFunc
    then do
      function
      program
    else do
      global_variable
      program

global_variable :: ExceptT Error (State ParserState) ()
global_variable = do
  ty <- declspec
  iter ty
  skip (Punct ";")
  where
    iter base = do
      (t, name) <- declarator base
      _ <- new_gvar name t
      tok <- seeHeadTokenKind
      case tok of
        Punct "," -> do
          skip (Punct ",")
          iter base
        _         -> pure ()


parse :: [Token] -> Either Error ([Obj], [Token], (IntMap Obj))
parse toks = do
  let initState = ParserState
        { tokens      = toks
        , currentVars = ([], [])
        , allObjects  = IntMap.empty
        , uniqCounter = 0
        , scopes      = []
        }
  let (r, newState) = runState (runExceptT program) initState
  let toks = tokens newState
  let globals = (snd . currentVars)  newState
  let storage = allObjects newState
  case r of
    Left e -> Left e
    Right _ -> return (globals, toks, storage)
