module Codegen where
import System.IO
import Text.Printf
import RBCC
import Data.Char

push :: Int -> IO (Int)
push depth = do
  printf "  push %%rax\n"
  return (depth +1)

pop :: Int -> String ->  IO (Int)
pop depth text= do
  printf "  pop %s\n" text
  return (depth-1)

gen_addr :: Node -> IO ()
gen_addr (VAR (a:[])) = do
  let offset = (ord a - ord 'a' +1) * 8
  printf "  lea %d(%%rbp), %%rax\n" ( - offset)

gen_addr _ = do
  hPutStrLn stderr "not a value"

gen_expr ::Int -> Node -> IO (Int)
gen_expr depth node@(VAR _) = do
  gen_addr node
  printf "  mov (%%rax), %%rax\n"
  return depth

gen_expr depth (BIN_OP Assign lhs rhs) = do
  gen_addr lhs
  depth <- push depth
  depth <- gen_expr depth rhs
  depth <- pop depth "%rdi"
  printf "  mov %%rax, (%%rdi)\n"
  return depth

gen_expr depth (NUM a) = do
  printf "  mov $%d, %%rax\n" a
  return depth

gen_expr depth (UNARY Neg a) = do
  depth <- gen_expr depth a
  printf "  neg %%rax\n"
  return depth

gen_expr depth (BIN_OP op lhs rhs) = do
  depth <- gen_expr depth rhs
  depth <- push depth
  depth <- gen_expr depth lhs
  depth <- pop depth "%rdi"
  gen_bin_op op
  return depth

gen_expr depth _ = do
  hPutStrLn stderr "invalid expression"
  return depth

gen_stmt :: Node -> IO (Int)
gen_stmt (EXPS_STMT []) = do return 0

gen_stmt (EXPS_STMT (n:ns)) = do
  depth <- gen_expr 0 n
  let _ = assert (depth == 0) 0
  gen_stmt (EXPS_STMT ns)

gen_stmt _ = do
  hPutStrLn stderr "invalid statement"
  return 1


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

gen_bin_op ND_EQ = do
  printf " cmp %%rdi, %%rax\n"
  printf " sete %%al\n"
gen_bin_op ND_NE = do
  printf " cmp %%rdi, %%rax\n"
  printf " setne %%al\n"
gen_bin_op ND_LT = do
  printf " cmp %%rdi, %%rax\n"
  printf " setl %%al\n"
gen_bin_op ND_LE = do
  printf " cmp %%rdi, %%rax\n"
  printf " setle %%al\n"


assert :: Bool -> a -> a
assert False _ = error "assertion failed!"
assert _     x = x


codegen ::Node -> IO (Int)
codegen node = do
  printf "  .globl main\n"
  printf "main:\n"
  -- Prologue
  printf "  push %%rbp\n"
  printf "  mov %%rsp, %%rbp\n"
  printf "  sub $208, %%rsp\n"
  -- Traverse the AST to emit assembly
  _ <- gen_stmt  node
  printf "  mov %%rbp, %%rsp\n"
  printf "  pop %%rbp\n"
  printf("  ret\n");
  return 0
