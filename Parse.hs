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


toPunct (str, op) = (Punct str, op)

type ParserState = ([Token], [Obj])

stmt          :: ExceptT Error (State ParserState) Node
expr_stmt     :: ExceptT Error (State ParserState) Node
primary       :: ExceptT Error (State ParserState) Node
unary         :: ExceptT Error (State ParserState) Node
assign        :: ExceptT Error (State ParserState) Node
compound_stmt :: ExceptT Error (State ParserState) Node
expr          :: ExceptT Error (State ParserState) Node

equality      :: ExceptT Error (State ParserState) Node
relational    :: ExceptT Error (State ParserState) Node
add           :: ExceptT Error (State ParserState) Node
mul           :: ExceptT Error (State ParserState) Node


join_bin ::
   ExceptT Error (State ParserState) Node
   -> [(String, (Node -> Node -> Node))]
   -> ExceptT Error (State ParserState) Node


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



join_bin sub bin_ops = do
  node <- sub
  join_ node
  where
    join_ lhs = do
      toks <- getTokens
      let tokKind = (tokenKind . head) toks
      case lookup tokKind (map toPunct bin_ops) of
        Just op -> do
          l <- getTokens
          putTokens (tail l)
          rhs <- sub
          join_ (op lhs rhs)
        Nothing ->  return lhs



equality   = join_bin relational [("==", BIN_OP ND_EQ), ("!=", BIN_OP ND_NE)]
relational = join_bin add
  [
    ("<",  BIN_OP ND_LT),
    ("<=", BIN_OP ND_LE),
    (">",  flip (BIN_OP ND_LT)),
    (">=", flip (BIN_OP ND_LE))
  ]

add        = join_bin mul        [("+", BIN_OP Add), ("-", BIN_OP Sub)]
mul        = join_bin unary      [("*", BIN_OP Mul), ("/", BIN_OP Div)]

assign = do
  lhs <- equality
  ts <- getTokens
  if head_equal ts (Punct "=")
  then do
    putTokens (tail ts)
    rhs <- assign
    return $ BIN_OP Assign lhs rhs
  else return lhs

expr       = assign
unary = do
  toks@(t: ts) <- getTokens
  if head_equal toks (Punct "+")
  then do
    putTokens ts
    unary
  else if head_equal toks (Punct "-")
  then do
      putTokens ts
      node <- unary
      return $ UNARY Neg node

  else primary


find_var :: String -> ExceptT Error (State ParserState) (Maybe Obj)
find_var var = do
  vars <- getLocals
  return (find f vars) where
    f (Obj name _) = var == name

new_lvar :: String -> ExceptT Error (State ParserState) Obj
new_lvar name = do
  vars <- getLocals
  let v = Obj name 0
  putLocals (v:vars)
  return v

  
-- primary = "(" expr ")" | ident | num
primary = do
  (t: ts) <- getTokens
  putTokens ts
  case t of
    (Token (Num v) _ _) -> do
       return $ NUM v
    (Token (Ident str) _ _) -> do
      fv <- find_var str
      case fv of
        Nothing -> do
          var <- new_lvar str
          return (VAR var)
        Just var -> return (VAR var)
    (Token (Punct "(") _ _) -> do
      node <- expr
      skip (Punct ")")
      return node
    _ -> throwE (ErrorToken t "expected an expression")


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
    _ <- popHeadToken
    return $BLOCK []
  else do
    node <- expr
    skip (Punct ";")
    return (EXPS_STMT [node])


compound_stmt  = do
  nodes <- iter []
  return $ BLOCK nodes
  where
    iter nodes = do
      endBlock <- head_equalM (Punct "}")
      if endBlock
      then do
        _ <- popHeadToken
        return nodes
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
    putTokens (tail ts)
    node <-expr
    skip (Punct ";")
    return (UNARY Return node)
  else if head_equal ts (Keyword "while")
  then do
    _ <- popHeadToken
    skip (Punct "(")
    cond <- expr
    skip (Punct ")")
    body <- stmt
    return (FOR Nothing (Just cond) Nothing body)
  else if head_equal ts (Keyword "for")
  then do
    _ <- popHeadToken
    skip (Punct "(")

    ini <- expr_stmt
    cond <- maybe_expr (Punct ";")
    skip (Punct ";")

    inc<- maybe_expr (Punct ")")
    skip (Punct ")")

    body <- stmt
    return (FOR (Just ini) cond inc body)
  else if head_equal ts (Keyword "if")
  then do
    _ <- popHeadToken
    skip (Punct "(")
    cond <- expr
    skip (Punct ")")
    _then <- stmt

    elseBlock <- head_equalM (Keyword "else")
    if elseBlock
    then do
      _ <- popHeadToken
      _else <- stmt
      return (IF cond _then (Just _else))
    else return (IF cond _then Nothing)
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
