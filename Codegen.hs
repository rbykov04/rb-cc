module Codegen where
import Text.Printf
import RBCC

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
  -- Traverse the AST to emit assembly
  depth <- gen_expr 0 node
  printf("  ret\n");
  return $ assert (depth == 0) 0