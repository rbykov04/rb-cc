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

data Node =
  NUM Int
  | ADD Node Node
  | SUB Node Node
  | MUL Node Node
  | DIV Node Node
  deriving (Show, Eq)

expr    :: String -> [Token] -> IO (Either Int (Node, [Token]))
mul     :: String -> [Token] -> IO (Either Int (Node, [Token]))
primary :: String -> [Token] -> IO (Either Int (Node, [Token]))

head_equal :: [Token] -> TokenKind -> Bool
head_equal ((Token (Punct a) _ _) : _) (Punct b) = a == b
head_equal ((Token EOF _ _) : _) EOF = True
head_equal [] _ = False
head_equal _ (Num x) = False
head_equal _ (Punct b) = False
head_equal _ _ = False

join_bin sub bin_ops = parser_t where
  ops = map toPunct bin_ops
  toPunct (str, op) = (Punct str, op)
  parser_t input toks = do
      node <- sub input toks
      case node of
        Left code -> return (Left code)
        Right (node, ts) -> join input node ts

  join input lhs toks = (apply_binary . tokenKind . head) toks where
    apply_binary t = case lookup t ops of
      Just op -> do
        Right (rhs, ts) <- sub input (tail toks)
        join input (op lhs rhs) ts
      Nothing -> return $ Right (lhs, toks)


expr = join_bin mul     [("+", ADD), ("-", SUB)]
mul  = join_bin primary [("*", MUL), ("/", DIV)]

skip :: String -> [Token] -> TokenKind -> IO (Either Int [Token])
skip input (t:ts) tok
  | head_equal (t:ts) tok = return $ Right ts
  | otherwise = do
      error_tok input t $ "expected " ++ show tok
      return $ Left 1

-- primay = "(" expr ")" | num
primary input ((Token (Num v) _ _): ts) = do
  return (Right (NUM v, ts))

primary input toks@(t:ts)
  | head_equal toks (Punct "(") = do
      Right (node, tss) <- expr input ts
      Right tsss <- skip input tss (Punct ")")
      return $ Right (node, tsss)

  | otherwise = do
    code <- error_tok input t "expected an expression"
    return $ Left code

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

gen_expr depth (ADD lhs rhs) = do
  gen_expr depth rhs
  depth <- push depth
  gen_expr depth lhs
  depth <- pop depth "%rdi"
  printf "  add %%rdi, %%rax\n"
  return depth

gen_expr depth (SUB lhs rhs) = do
  gen_expr depth rhs
  depth <- push depth
  gen_expr depth lhs
  depth <- pop depth "%rdi"
  printf "  sub %%rdi, %%rax\n"
  return depth

gen_expr depth (MUL lhs rhs) = do
  gen_expr depth rhs
  depth <- push depth
  gen_expr depth lhs
  depth <- pop depth "%rdi"
  printf "  imul %%rdi, %%rax\n"
  return depth

gen_expr depth (DIV lhs rhs) = do
  gen_expr depth rhs
  depth <- push depth
  gen_expr depth lhs
  depth <- pop depth "%rdi"
  printf "  cqo\n"
  printf "  idiv %%rdi\n"
  return depth

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
        Right (node, ts) <- expr p toks
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
