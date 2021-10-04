module Parse where
import System.Environment
import System.IO
import RBCC
import Codegen
import Tokenize

expr       :: [Token] -> Either Error (Node, [Token])
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

expr       = equality
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

-- primay = "(" expr ")" | num
primary ((Token (Num v) _ _): ts) = Right (NUM v, ts)

primary toks@(t:ts)
  | head_equal toks (Punct "(") = do
      (node, tss) <- expr ts
      tsss <- skip tss (Punct ")")
      return (node, tsss)

  | otherwise = Left (ErrorToken t "expected an expression")

parse :: [Token] -> Either Error (Node, [Token])
parse toks = do
  (node, ts) <- expr toks
  if not $ head_equal ts EOF
  then Left (ErrorToken (head ts) "extra token")
  else return (node, ts)
