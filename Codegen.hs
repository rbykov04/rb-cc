module Codegen where
import RBCC
import Data.List
import Control.Monad.Trans.Except
import Control.Monad.State

type CodegenError = String
type CodegenState = [String]

genLine :: String -> ExceptT CodegenError (State CodegenState) ()
genLine prog = do
  r <- get
  put (r ++ [prog])

genLines :: [String] -> ExceptT CodegenError (State CodegenState) ()
genLines prog = do
  r <- get
  put (r ++ prog)


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

convert :: Either CodegenError CodegenState -> ExceptT CodegenError (State CodegenState) ()
convert e = do
  case e of
    Right prog -> genLines prog
    Left err -> throwE err

convertEx :: Either CodegenError (d, CodegenState) -> ExceptT CodegenError (State CodegenState) d
convertEx e = do
  case e of
    Right (d, prog) -> do
      genLines prog
      return d
    Left err -> throwE err

gen_expr ::Int -> Node ->[Obj]-> ExceptT CodegenError (State CodegenState) Int
gen_expr depth node@(VAR _) locals = do
  convert $ gen_addr node locals
  genLine "  mov (%rax), %rax\n"
  return depth

gen_expr depth (BIN_OP Assign lhs rhs) locals = do
  convert $ gen_addr lhs locals
  depth <- convertEx $ Right $push depth
  depth <- gen_expr depth rhs locals
  depth <- convertEx $ Right $ pop "%rdi" depth
  genLine "  mov %rax, (%rdi)\n"
  return depth

gen_expr depth (NUM a) _ = do
  genLine $ "  mov $" ++ show a ++ ", %rax\n"
  return depth

gen_expr depth (UNARY Neg a) locals = do
  depth <- gen_expr depth a locals
  genLine "  neg %rax\n"
  return depth

gen_expr depth (BIN_OP op lhs rhs) locals = do
  depth <- gen_expr depth rhs locals
  depth <- convertEx $ Right $ push depth
  depth <- gen_expr depth lhs locals
  depth <- convertEx $ Right $ pop "%rdi" depth
  genLines $gen_bin_op op
  return depth

gen_expr depth t _= throwE $ "invalid expression" ++ show t

gen_stmt :: Node -> [Obj] -> ExceptT CodegenError (State CodegenState) ()
gen_stmt (EXPS_STMT []) _ = return ()

gen_stmt (EXPS_STMT (n:ns)) locals= do
  d <- gen_expr 0 n locals
  let _ = assert (d == 0) 0
  gen_stmt (EXPS_STMT ns) locals

gen_stmt (BLOCK nodes) locals =
    iter nodes
    where
      iter [] = return ()
      iter (n:ns) = do
        gen_stmt n locals
        iter ns

gen_stmt (UNARY Return node) locals = do
  d <- gen_expr 0 node locals
  let _ = assert (d == 0) 0
  genLine "  jmp .L.return\n"

gen_stmt t _ = throwE $ "gen stmt: invalid statement " ++ show t

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

gen_block :: [Node] -> [Obj] -> ExceptT CodegenError (State CodegenState) ()
gen_block [] _ = return ()
gen_block (n:ns) l = do
  gen_stmt n l
  gen_block ns l

codegen_ :: Function -> ExceptT CodegenError (State CodegenState) ()
codegen_ f = do
  let f' = assign_lvar_offset f
  let locals = functionLocals f'
  let body = functionBody f'
  let stack_size = functionStackSize f'

  genLine "  .globl main\n"
  genLine "main:\n"
  -- Prologue
  genLine "  push %rbp\n"
  genLine "  mov %rsp, %rbp\n"
  genLine $ "  sub $" ++ show stack_size ++", %rsp\n"

  -- Traverse the AST to emit assembly
  gen_block body locals

  genLine ".L.return:\n"
  genLine "  mov %rbp, %rsp\n"
  genLine "  pop %rbp\n"
  genLine "  ret\n"


codegen :: Function -> Either String [String]
codegen f = do
  let (r,s') = runState (runExceptT (codegen_ f)) []
  case r of
    Left e -> Left e
    Right _ -> return s'
