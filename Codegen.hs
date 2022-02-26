module Codegen where
import System.IO
import Text.Printf
import RBCC
import Data.Char
import Data.List

push :: Int -> IO (Int)
push depth = do
  printf "  push %%rax\n"
  return (depth +1)

pop :: Int -> String ->  IO (Int)
pop depth text= do
  printf "  pop %s\n" text
  return (depth-1)

gen_addr :: Node ->[Obj] -> IO ()
gen_addr (VAR (Obj var _)) locals = do
  case find f locals of
    Nothing -> do
      hPutStrLn stderr $ "Codegen: variable " ++ var ++ " is not declare"
    Just (Obj _ offset ) -> do
      printf "  lea %d(%%rbp), %%rax\n" offset
    where
      f (Obj name _) = var == name

gen_addr _ _ = do
  hPutStrLn stderr "not a value"

gen_expr ::Int -> Node ->[Obj]-> IO (Int)
gen_expr depth node@(VAR _) locals= do
  gen_addr node locals
  printf "  mov (%%rax), %%rax\n"
  return depth

gen_expr depth (BIN_OP Assign lhs rhs) locals = do
  gen_addr lhs locals
  depth <- push depth
  depth <- gen_expr depth rhs locals
  depth <- pop depth "%rdi"
  printf "  mov %%rax, (%%rdi)\n"
  return depth

gen_expr depth (NUM a) locals = do
  printf "  mov $%d, %%rax\n" a
  return depth

gen_expr depth (UNARY Neg a) locals = do
  depth <- gen_expr depth a locals
  printf "  neg %%rax\n"
  return depth

gen_expr depth (BIN_OP op lhs rhs) locals = do
  depth <- gen_expr depth rhs locals
  depth <- push depth
  depth <- gen_expr depth lhs locals
  depth <- pop depth "%rdi"
  gen_bin_op op
  return depth

gen_expr depth t _= do
  hPutStrLn stderr $ "invalid expression" ++ show t
  return depth

gen_stmt :: Node -> [Obj] -> IO (Int)
gen_stmt (EXPS_STMT []) _ = do return 0

gen_stmt (EXPS_STMT (n:ns)) locals = do
  depth <- gen_expr 0 n locals
  let _ = assert (depth == 0) 0
  gen_stmt (EXPS_STMT ns) locals

gen_stmt (BLOCK nodes) locals = do
    iter nodes
    where
      iter [] = do
        return 0
      iter (n:ns) = do
        _ <- gen_stmt n locals
        iter ns



gen_stmt (UNARY Return node) locals = do
    r <- gen_expr 0 node locals
    printf "  jmp .L.return\n"
    return r

gen_stmt t _= do
  hPutStrLn stderr $ "gen stmt: invalid statement " ++ show t
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

-- Round up `n` to the nearest multiple of `align`. For instance,
-- align_to 5 8  returns 8 and align_to 11  8  returns 16.
align_to :: Int -> Int -> Int
align_to n align = ((n + align - 1) `div` align) * align


assign_lvar_offset :: Function -> Function
assign_lvar_offset (Function node vars _) = Function node vars' (align_to offset' 16)
  where
    (vars', offset') = f vars 0
    f [] r = ([], r)
    f ((Obj name _):vs) offset = ((Obj name (0 - offset)) : vs', offset'') where
      (vs', offset'') = f vs (offset + 8)



gen_block :: [Node] -> [Obj] -> IO (Int)
gen_block [] _ = return 0
gen_block (n:ns) l = do
  _ <- gen_stmt n l
  gen_block ns l


codegen :: Function -> IO (Int)
codegen f = do
  let f' = assign_lvar_offset f
  let locals = functionLocals f'
  let body = functionBody f'
  let stack_size = functionStackSize f'
  printf "  .globl main\n"
  printf "main:\n"
  -- Prologue
  printf "  push %%rbp\n"
  printf "  mov %%rsp, %%rbp\n"
  printf "  sub $%d, %%rsp\n" stack_size
  -- Traverse the AST to emit assembly
  _ <- gen_block  body locals

  printf ".L.return:\n"
  printf "  mov %%rbp, %%rsp\n"
  printf "  pop %%rbp\n"
  printf("  ret\n");
  return 0
