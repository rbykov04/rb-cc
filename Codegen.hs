module Codegen where
import RBCC
import Data.List
import Control.Monad.Trans.Except
import Control.Monad.State

type CodegenError = Error
type CodegenState = ([String], Int)

genLine :: String -> ExceptT CodegenError (State CodegenState) ()
genLine prog = do
  r <- get
  put (fst r ++ [prog], snd r)

genLines :: [String] -> ExceptT CodegenError (State CodegenState) ()
genLines prog = do
  r <- get
  put (fst r ++ prog, snd r)

getCount :: ExceptT CodegenError (State CodegenState) Int
getCount = do
  r <- get
  put (fst r, snd r + 1)
  return $ snd r


push :: Int -> (Int, [String])
push depth = (depth + 1, ["  push %rax\n"])


pop :: String -> Int -> (Int, [String])
pop text depth = (depth - 1, ["  pop "++text ++ "\n"])


gen_addr :: Node ->[Obj]-> ExceptT CodegenError (State CodegenState) ()
gen_addr (Node (VAR (Obj var _ _)) _ tok) locals = do
  case find f locals of
    Nothing ->  throwE $ ErrorToken tok ("variable " ++ var ++ " is not declare")
    Just (Obj _ _ offset ) -> do
      genLine $ "  lea " ++ show offset ++ "(%rbp), %rax\n"
    where
      f (Obj name _ _) = var == name

gen_addr (Node (UNARY Deref node) _ _) locals = do
  _ <- gen_expr 0 node locals
  return ()

gen_addr (Node _ _ tok) _ = throwE $ ErrorToken tok "Codegen: not a value"

convert :: Either CodegenError [String] -> ExceptT CodegenError (State CodegenState) ()
convert e = do
  case e of
    Right prog -> genLines prog
    Left err -> throwE err

convertEx :: Either CodegenError (d, [String]) -> ExceptT CodegenError (State CodegenState) d
convertEx e = do
  case e of
    Right (d, prog) -> do
      genLines prog
      return d
    Left err -> throwE err

argreg :: [String]
--argreg = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]
argreg = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]

gen_expr ::Int -> Node ->[Obj]-> ExceptT CodegenError (State CodegenState) Int
gen_expr depth node@(Node (VAR _) _ tok) locals = do
  gen_addr node locals
  genLine "  mov (%rax), %rax\n"
  return depth

gen_expr depth (Node (BIN_OP Assign lhs rhs) _ tok) locals = do
  gen_addr lhs locals
  depth <- convertEx $ Right $push depth
  depth <- gen_expr depth rhs locals
  depth <- convertEx $ Right $ pop "%rdi" depth
  genLine "  mov %rax, (%rdi)\n"
  return depth

gen_expr depth (Node (FUNCALL name arguments) _ tok) locals = do
  let nargs = length arguments
  let fregs = reverse $ take nargs argreg
  depth <- gen_args depth arguments
  depth <- pop_reg depth fregs nargs

  genLine $"  mov $0, %rax\n"
  genLine $"  call "++ name ++ "\n"

  return depth
  where
    pop_reg depth _ 0 = return depth
    pop_reg depth (r:regs) count  = do
      depth <- convertEx $ Right $ pop r depth
      pop_reg depth regs (count - 1)

    gen_args depth []         = return depth
    gen_args depth (arg:args) = do
      depth <- gen_expr depth arg locals
      depth <- convertEx $ Right $ push depth
      gen_args depth args

gen_expr depth (Node (NUM a) _ tok) _ = do
  genLine $ "  mov $" ++ show a ++ ", %rax\n"
  return depth

gen_expr depth (Node (UNARY Neg a) _ tok) locals = do
  depth <- gen_expr depth a locals
  genLine "  neg %rax\n"
  return depth

gen_expr depth (Node (UNARY Addr node) _ _) locals = do
  gen_addr node locals
  return depth

gen_expr depth (Node (UNARY Deref node) _ _) locals = do
  depth <- gen_expr depth node locals
  genLine "  mov (%rax), %rax\n"
  return depth

gen_expr depth (Node (BIN_OP op lhs rhs) _ tok) locals = do
  depth <- gen_expr depth rhs locals
  depth <- convertEx $ Right $ push depth
  depth <- gen_expr depth lhs locals
  depth <- convertEx $ Right $ pop "%rdi" depth
  genLines $gen_bin_op op
  return depth

gen_expr depth (Node _ _ tok) _= throwE $ ErrorToken tok "Codegen: invalid expression"


gen_stmt :: Node -> [Obj] -> ExceptT CodegenError (State CodegenState) ()
gen_stmt (Node (EXPS_STMT []) _ tok) _ = return ()

gen_stmt (Node (EXPS_STMT (n:ns)) t tok) locals= do
  d <- gen_expr 0 n locals
  let _ = assert (d == 0) 0
  gen_stmt (Node (EXPS_STMT ns) t tok) locals

gen_stmt (Node (BLOCK nodes) _ tok) locals =
    iter nodes
    where
      iter [] = return ()
      iter (n:ns) = do
        gen_stmt n locals
        iter ns


gen_stmt (Node (IF cond then_ else_) _ tok) locals = do
  c <- getCount
  _ <- gen_expr 0 cond locals

  genLine       "  cmp $0, %rax\n"
  genLine $     "  je  .L.else."++ show c ++"\n"
  gen_stmt then_ locals
  genLine $     "  jmp .L.end." ++ show c ++ "\n"
  genLine $     ".L.else." ++ show c++":\n"

  case else_ of
    Just node -> do
      gen_stmt node locals
      genLine $ ".L.end."++ show c ++ ":\n"
    Nothing ->
      genLine $ ".L.end."++ show c ++ ":\n"

gen_stmt (Node (FOR ini cond inc body) _ tok) locals = do
  c <- getCount
  genInit
  genLine $     ".L.begin." ++ show c ++ ":\n"
  genCond c
  gen_stmt body locals
  genInc
  genLine $ "  jmp .L.begin." ++ show c ++ "\n"
  genLine $ ".L.end." ++ show c ++ ":\n"
  where
    genInit = do
      case ini of
        Nothing -> return ()
        Just node -> do
          gen_stmt node locals
    genCond c = do
      case cond of
        Nothing -> return ()
        Just node -> do
          _ <- gen_expr 0 node locals
          genLine $ "  cmp $0, %rax\n"
          genLine $ "  je  .L.end." ++ show c ++"\n"
    genInc = do
      case inc of
        Nothing -> return ()
        Just node -> do
          _ <- gen_expr 0 node locals
          return ()



gen_stmt (Node (RETURN node) _ tok) locals = do
  d <- gen_expr 0 node locals
  let _ = assert (d == 0) 0
  genLine "  jmp .L.return\n"

gen_stmt (Node _ _ tok) _ = throwE $ ErrorToken tok ("gen stmt: invalid statement ")

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
    f ((Obj name t _):vs) offset = ((Obj name t (0 - offset)) : vs', offset'') where
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


codegen :: Function -> Either Error [String]
codegen f = do
  let (r,s') = runState (runExceptT (codegen_ f)) ([], 1)
  case r of
    Left e -> Left e
    Right _ -> return (fst s')
