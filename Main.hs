module Main where
import System.Environment
import System.IO
import Text.Printf
import Data.Char

strtol :: Int -> String -> (Int, String, Int)
strtol = helper 0 where
  helper res c [] = (res,[], c)
  helper res 0 xs = (res, xs, 0)
  helper res count (x:xs)
    | isDigit x = helper (res* 10 + digitToInt x) (count-1) xs
    | otherwise = (res, x:xs, count)

--
-- Tokenizer
--

data TokenKind =
  EOF
  | Punct String
  | Num Int
  deriving (Eq, Show)

data Token = Token
  {
    tokenKind :: TokenKind,
    tokenLen :: Int,
    tokenLoc :: Int
  }
  deriving (Eq, Show)

error_at :: String -> Int -> String -> IO (Int)
error_at current_input loc text = do
  hPutStrLn stderr $ current_input
  hPutStrLn stderr $ replicate loc ' ' ++ "^ " ++ text
  return 1

error_tok :: String -> Token -> String -> IO (Int)
error_tok input tok = error_at input (tokenLoc tok)

tokenize :: Int -> String -> [Either (Int, String) Token]
tokenize c [] = [Right$ Token EOF 0 c]
tokenize c (p:ps)
  | isSeparator p = tokenize (c+1) ps
  | isPunctuation p || isSymbol p  =(Right $ Token (Punct [p]) 1 c) : tokenize (c+1) ps
  | isDigit p       = (Right $ Token (Num number) len c) : tokenize (c+len) pss
  | otherwise = [Left (c, "invalid token")]
  where
    (number, pss, x) = strtol 10 (p:ps)
    len = 10 - x

get_number :: String -> Token -> IO (Maybe Int)
get_number input (Token (Num v) _ _) = return (Just v)
get_number input tok = do
  error_tok input tok "expected a number"
  return Nothing

--
-- Parser
--
data Error = ErrorCode Int | ErrorText String | ErrorToken Token String deriving Show
data BinOp = Add | Sub | Mul | Div deriving (Show, Eq)
data UnOp = Neg deriving (Show, Eq)

data Node =
  NUM Int
  | BIN_OP BinOp Node Node
  | UNARY UnOp Node
  deriving (Show, Eq)

expr    :: [Token] -> Either Error (Node, [Token])
mul     :: [Token] -> Either Error (Node, [Token])
unary   :: [Token] -> Either Error (Node, [Token])
primary :: [Token] -> Either Error (Node, [Token])

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

expr = join_bin mul     [("+", BIN_OP Add), ("-", BIN_OP Sub)]
mul  = join_bin unary [("*", BIN_OP Mul), ("/", BIN_OP Div)]

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

printError :: String -> Error -> IO (Int)
printError input (ErrorToken t text) = do
  error_tok input t text
--
-- Code generator
--
push :: Int -> IO (Int)
push depth = do
  printf "  push %%rax\n"
  return (depth +1)

pop :: Int -> String ->  IO (Int)
pop depth text= do
  printf "  pop %s\n" text
  return (depth-1)

gen_expr ::Int -> Node -> IO (Int)
gen_expr depth (NUM a) = do
  printf "  mov $%d, %%rax\n" a
  return depth

gen_expr depth (UNARY Neg a) = do
  gen_expr depth a
  printf "  neg %%rax\n"
  return depth

gen_expr depth (BIN_OP op lhs rhs) = do
  gen_expr depth rhs
  depth <- push depth
  gen_expr depth lhs
  depth <- pop depth "%rdi"
  gen_bin_op op
  return depth

gen_bin_op :: BinOp -> IO ()
gen_bin_op Add = do
  printf "  add %%rdi, %%rax\n"
gen_bin_op Sub = do
  printf "  sub %%rdi, %%rax\n"
gen_bin_op Mul = do
  printf "  imul %%rdi, %%rax\n"
gen_bin_op Div = do
  printf "  cqo\n"
  printf "  idiv %%rdi\n"


assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

main :: IO (Int)
main = do
  args <- getArgs
  if length args < 1 then do
    hPutStrLn stderr "rb-cc: invalid number of arguments"
    return 1
  else do
    let p = head args
    let res = sequence $ tokenize 0 p
    case res of
      Left (loc, text) -> do
        error_at p loc text
      Right toks -> do
        let parse_res = expr toks
        case parse_res of
          Left err -> printError p err
          Right (node, ts) ->
            if not $ head_equal ts EOF
            then do
              error_tok p (head ts) "extra token"
              return 1
            else do
              printf "  .globl main\n"
              printf "main:\n"
              -- Traverse the AST to emit assembly
              depth <- gen_expr 0 node
              printf("  ret\n");
              return (assert (depth == 0) 0)
