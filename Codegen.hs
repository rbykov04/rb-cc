module Codegen where
import System.IO
import Text.Printf
import RBCC
import Data.Char
import Data.List

printProgram :: [String] -> IO ()
printProgram [] = return ()
printProgram (s:ss) = do
  printf "%s" s
  printProgram ss

push :: Int -> (Int, [String])
push depth = (depth + 1, ["  push %rax\n"])


pop :: String -> Int -> (Int, [String])
pop text depth = (depth - 1, ["  pop "++text ++ "\n"])

gen_addr :: Node ->[Obj] -> Either String [String]
gen_addr (VAR (Obj var _)) locals =
  case find f locals of
    Nothing -> Left $ "variable " ++ var ++ " is not declare"
    Just (Obj _ offset ) ->
      Right $ ["  lea " ++ show offset ++ "(%rbp), %rax\n"]
    where
      f (Obj name _) = var == name
gen_addr _ _ = Left $ "not a value"

gen_expr ::Int -> Node ->[Obj]-> Either String (Int, [String])
gen_expr depth node@(VAR _) locals = do
  prog <- gen_addr node locals
  return (depth, prog ++ ["  mov (%rax), %rax\n"])

gen_expr depth (BIN_OP Assign lhs rhs) locals = do
  addr_ <- gen_addr lhs locals
  let (depth, push_) = push depth
  (depth, expr_) <- gen_expr depth rhs locals
  let (depth, pop_) = pop "%rdi" depth

  let end ="  mov %rax, (%rdi)\n"
  return (depth, addr_ ++ push_ ++ expr_ ++ pop_ ++ [end])

gen_expr depth (NUM a) _ = do
  return (depth, ["  mov $" ++ show a ++ ", %rax\n"])

gen_expr depth (UNARY Neg a) locals = do
  (depth, prog) <- gen_expr depth a locals

  return (depth, prog ++ ["  neg %rax\n"])

gen_expr depth (BIN_OP op lhs rhs) locals = do
  (depth, rhs) <- gen_expr depth rhs locals

  let (depth, push_) = push depth
  (depth, lhs)<- gen_expr depth lhs locals
  let (depth, pop_) = pop "%rdi" depth
  let op_ =gen_bin_op op
  return (depth, rhs ++ push_ ++ lhs ++ pop_ ++ op_)

gen_expr depth t _= Left $ "invalid expression" ++ show t

gen_stmt :: Node -> [Obj] -> Either String [String]
gen_stmt (EXPS_STMT []) _ = return []

gen_stmt (EXPS_STMT (n:ns)) locals= do
  (d, expr) <- gen_expr 0 n locals
  let _ = assert (d == 0) 0
  stmt <- gen_stmt (EXPS_STMT ns) locals
  return (expr ++ stmt)

gen_stmt (BLOCK nodes) locals =
    iter nodes
    where
      iter [] = do
        return []
      iter (n:ns) = do
        stmt <- gen_stmt n locals
        stmts <- iter ns
        return (stmt ++ stmts)

gen_stmt (UNARY Return node) locals = do
    (d, expr) <- gen_expr 0 node locals
    let _ = assert (d == 0) 0
    return (expr ++ ["  jmp .L.return\n"])

gen_stmt t _ = Left $ "gen stmt: invalid statement " ++ show t

gen_bin_op :: BinOp -> [String]
gen_bin_op Add =   ["  add %rdi, %rax\n"]
gen_bin_op Sub =   ["  sub %rdi, %rax\n"]
gen_bin_op Mul =   ["  imul %rdi, %rax\n"]
gen_bin_op Div =   ["  cqo\n",
                    "  idiv %rdi\n" ]

gen_bin_op ND_EQ = [" cmp %rdi, %rax\n",
                    " sete %al\n"]
gen_bin_op ND_NE = [" cmp %rdi, %rax\n",
                    " setne %al\n"]
gen_bin_op ND_LT = [" cmp %rdi, %rax\n",
                    " setl %al\n"]
gen_bin_op ND_LE = [" cmp %rdi, %rax\n",
                    " setle %al\n"]


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



gen_block :: [Node] -> [Obj] -> Either String [String]
gen_block [] _ = do
  return []
gen_block (n:ns) l = do
  stmt <- gen_stmt n l
  prog <- gen_block ns l
  return (stmt ++ prog)

codegen :: Function -> Either String [String]
codegen f = do
  let f' = assign_lvar_offset f
  let locals = functionLocals f'
  let body = functionBody f'
  let stack_size = functionStackSize f'

  let begin = ["  .globl main\n",
               "main:\n",
                -- Prologue
                "  push %rbp\n",
                "  mov %rsp, %rbp\n",
                "  sub $" ++ show stack_size ++", %rsp\n"]
  -- Traverse the AST to emit assembly
  prog <- gen_block  body locals

  let end =    [".L.return:\n",
                "  mov %rbp, %rsp\n",
                "  pop %rbp\n",
                "  ret\n"]
  return (begin ++ prog ++ end)

