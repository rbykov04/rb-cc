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
import RBCC
import Codegen
import Tokenize
import Type
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
postfix       :: ExceptT Error (State ParserState) Node

newUniqName :: ExceptT Error (State ParserState) String
newUniqName = do
  r <- get
  let id_ = forth' r
  put (fst' r, snd' r, thrd' r, forth' r + 1)
  return $ ".L.." ++ show id_

getTokens :: ExceptT Error (State ParserState) [Token]
getTokens = do
  r <- get
  return $ fst' r

putTokens :: [Token] -> ExceptT Error (State ParserState) ()
putTokens toks = do
  r <- get
  put (toks, snd' r, thrd' r, forth' r)


getLocals :: ExceptT Error (State ParserState) [Obj]
getLocals = do
  r <- get
  return $ (fst. snd') r

putLocals :: [Obj] -> ExceptT Error (State ParserState) ()
putLocals vars = do
  r <- get
  put (fst' r, (vars, (snd. snd') r), thrd' r, forth' r)

getGlobals :: ExceptT Error (State ParserState) [Obj]
getGlobals = do
  r <- get
  return $ (snd. snd') r

putGlobals :: [Obj] -> ExceptT Error (State ParserState) ()
putGlobals vars = do
  r <- get
  put (fst' r, (((fst. snd') r), vars), thrd' r, forth' r)

putVars :: IntMap Obj -> ExceptT Error (State ParserState) ()
putVars vars = do
  r <- get
  put (fst' r, snd' r, vars, forth' r)




join_bin ::
   ExceptT Error (State ParserState) Node
   -> [(String, Node->Node->Token -> ExceptT Error (State ParserState) Node)]
   -> ExceptT Error (State ParserState) Node

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

add_bin op lhs rhs tok = add_type (BIN_OP op lhs rhs) tok

mul        = join_bin unary      [("*", add_bin Mul), ("/", add_bin Div)]
add        = join_bin mul        [("+", new_add), ("-", new_sub)]
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

make_offset ty count tok = do
  base <- add_type (NUM ((typeSize) ty)) tok
  add_type (BIN_OP Mul count base) tok

ptr_math op ptr ty count token = do
  offset <- make_offset ty count token
  add_type (BIN_OP op ptr offset) token


new_add  :: Node->Node->Token -> ExceptT Error (State ParserState) Node
new_add lhs rhs tok
  | is_integer (nodeType rhs) && is_integer (nodeType lhs) = add_type (BIN_OP Add lhs rhs) tok
  | otherwise =  case ((get_ptr_base . nodeType) lhs, (get_ptr_base . nodeType) rhs) of

  ((Just tyBase), Nothing)  -> ptr_math Add lhs tyBase rhs tok
  (Nothing, (Just tyBase))  -> ptr_math Add rhs tyBase lhs tok

  -- ptr +  ptr
  _ ->  throwE (ErrorToken tok "invalid operands")

new_sub  :: Node->Node->Token -> ExceptT Error (State ParserState) Node
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
        add_type (UNARY op node) tok

find_var :: String -> ExceptT Error (State ParserState) (Maybe Obj)
find_var var = do
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
          add_type (NUM v) t
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
    add_type (BLOCK []) tok
  else do
    tok <- seeHeadToken
    node <- expr
    skip (Punct ";")
    add_type (EXPS_STMT node) tok

-- declspec = "int" | "char"
declspec   :: ExceptT Error (State ParserState) Type
declspec = do
  kind <- seeHeadTokenKind
  case kind of
    Keyword "char" -> skip (Keyword "char") >> return make_char
    _              -> skip (Keyword "int")  >> return make_int

create_param_lvars :: [(Type,String)] -> ExceptT Error (State ParserState) [Int]
create_param_lvars = mapM $ (uncurry . flip) new_lvar


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
          ptr <- new_add node idx tok
          node' <- add_type (UNARY Deref ptr) tok
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

is_type = do
  kind <- seeHeadTokenKind
  case kind of
    (Keyword "int")  -> return True
    (Keyword "char") -> return True
    _                -> return False
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
        isType <- is_type
        if isType
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


function :: ExceptT Error (State ParserState) ()
function = do
  putLocals []
  ty <- declspec
  (ftype, name) <- declarator ty
  skip (Punct "{")
  s <- compound_stmt
  let nodes = [s]

  locals <- getLocals
  key <- new_gvar name ftype
  update_var (updateFunc (map objKey locals) nodes) key
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
  let (r, (tokens, (_, globals), storage, _)) = runState (runExceptT program) (toks, ([], []), IntMap.empty, 0 )
  case r of
    Left e -> Left e
    Right _ -> return (globals, tokens, storage)
