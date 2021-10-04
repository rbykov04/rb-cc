module Parse where
import System.Environment
import System.IO
import RBCC
import Codegen
import Tokenize

expr       :: [Token] -> Either Error (Node, [Token])
expr_stmt  :: [Token] -> Either Error (Node, [Token])
assign     :: [Token] -> Either Error (Node, [Token])
equality   :: [Token] -> Either Error (Node, [Token])
relational :: [Token] -> Either Error (Node, [Token])
add        :: [Token] -> Either Error (Node, [Token])
mul        :: [Token] -> Either Error (Node, [Token])
unary      :: [Token] -> Either Error (Node, [Token])
primary    :: [Token] -> Either Error (Node, [Token])

head_equal :: [Token] -> TokenKind -> Bool
head_equal ((Token (Punct a) _ _) : _) (Punct b) = a == b
head_equal ((Token EOF _ _) : _) EOF = True
head_equal [] _ = False
head_equal _ (Num x) = False
head_equal _ (Punct b) = False
head_equal _ _ = False

toPunct (str, op) = (Punct str, op)

join_bin sub bin_ops toks = do
  (node, ts) <- sub toks
  join node ts
  where
    join lhs toks = (apply_binary . tokenKind . head) toks where
      apply_binary t = case lookup t (map toPunct bin_ops) of
        Just op -> do
          (rhs,ts) <- sub (tail toks)
          join (op lhs rhs) ts
        Nothing ->  Right (lhs, toks)

expr       = assign
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

assign toks = do
  (lhs, ts) <- equality toks
  if head_equal ts (Punct "=")
  then do
    (rhs,tss) <- assign (tail ts)
    return (BIN_OP Assign lhs rhs, tss)
  else return (lhs, ts)


unary toks@(t:ts)
  | head_equal toks (Punct "+") = unary ts

  | head_equal toks (Punct "-") = do
      (node,tss) <- unary ts
      return (UNARY Neg node, tss)

  | otherwise = primary toks


skip :: [Token] -> TokenKind -> Either Error [Token]
skip (t:ts) tok
  | head_equal (t:ts) tok = Right ts
  | otherwise = Left (ErrorToken t ("expected" ++ show tok))

-- primary = "(" expr ")" | ident | num
primary ((Token (Num v) _ _): ts)     = Right (NUM v, ts)
primary ((Token (Ident str) _ _): ts) = Right (VAR str, ts)

primary toks@(t:ts)
  | head_equal toks (Punct "(") = do
      (node, tss) <- expr ts
      tsss <- skip tss (Punct ")")
      return (node, tsss)

  | otherwise = Left (ErrorToken t "expected an expression")

expr_stmt toks = do
  (node, tss) <- expr toks
  tsss <- skip tss (Punct ";")
  return (EXPS_STMT [node], tsss)

stmt  = expr_stmt

program :: [Token] -> Either Error (Node, [Token])
program toks = do
  (EXPS_STMT node, ts) <- stmt toks
  if not $ head_equal ts EOF
  then do
    (EXPS_STMT nodes, tss) <- program ts
    return (EXPS_STMT (node ++ nodes), tss)
  else return (EXPS_STMT node, ts)

parse :: [Token] -> Either Error (Node, [Token])
parse toks = do
  (node, ts) <- program toks
  if not $ head_equal ts EOF
  then Left (ErrorToken (head ts) "extra token")
  else return (node, ts)
