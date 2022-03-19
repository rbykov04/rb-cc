module Codegen where
import RBCC
import Data.List
import Control.Monad.Trans.Except
import Control.Monad.State

type CodegenError = Error
type CodegenState = ([String], Int, Maybe Function)

setCurFunc :: Function -> ExceptT CodegenError (State CodegenState) ()
setCurFunc func = do
  (code, count, _) <- get
  put (code, count, Just func)

getCurFunc :: ExceptT CodegenError (State CodegenState) Function
getCurFunc  = do
  (_, _, myabe_f) <- get
  case myabe_f of
    Nothing -> throwE $ ErrorText "current func is NULL"
    Just f -> return f

getCurFuncName :: ExceptT CodegenError (State CodegenState) String
getCurFuncName  = fmap functionName getCurFunc

getCurLocals :: ExceptT CodegenError (State CodegenState) [Obj]
getCurLocals  = fmap functionLocals getCurFunc

genLine :: String -> ExceptT CodegenError (State CodegenState) ()
genLine prog = do
  (code, count, f) <- get
  put (code ++ [prog], count, f)

genLines :: [String] -> ExceptT CodegenError (State CodegenState) ()
genLines prog = do
  (code, count, f) <- get
  put (code ++ prog, count, f)

getCount :: ExceptT CodegenError (State CodegenState) Int
getCount = do
  (code, count, f) <- get
  put (code, count + 1, f)
  return $ count

push :: Int -> (Int, [String])
push depth = (depth + 1, ["  push %rax\n"])

pop :: String -> Int -> (Int, [String])
pop text depth = (depth - 1, ["  pop "++text ++ "\n"])

gen_addr :: Node -> ExceptT CodegenError (State CodegenState) ()
gen_addr (Node (VAR (Obj var _ _)) _ tok) = do
  locals <- getCurLocals
  case find f locals of
    Nothing ->  throwE $ ErrorToken tok ("variable " ++ var ++ " is not declare")
    Just (Obj _ _ offset ) -> do
      genLine $ "  lea " ++ show offset ++ "(%rbp), %rax\n"
    where
      f (Obj name _ _) = var == name

gen_addr (Node (UNARY Deref node) _ _) = do
  _ <- gen_expr 0 node
  return ()

gen_addr (Node _ _ tok) = throwE $ ErrorToken tok "Codegen: not a value"

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
argreg = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]

gen_expr ::Int -> Node ->ExceptT CodegenError (State CodegenState) Int
gen_expr depth node@(Node (VAR _) _ tok) = do
  gen_addr node
  genLine "  mov (%rax), %rax\n"
  return depth

gen_expr depth (Node (BIN_OP Assign lhs rhs) _ tok) = do
  gen_addr lhs
  depth <- convertEx $ Right $push depth
  depth <- gen_expr depth rhs
  depth <- convertEx $ Right $ pop "%rdi" depth
  genLine "  mov %rax, (%rdi)\n"
  return depth

gen_expr depth (Node (FUNCALL name arguments) _ tok) = do
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
      depth <- gen_expr depth arg
      depth <- convertEx $ Right $ push depth
      gen_args depth args

gen_expr depth (Node (NUM a) _ tok) = do
  genLine $ "  mov $" ++ show a ++ ", %rax\n"
  return depth

gen_expr depth (Node (UNARY Neg a) _ tok) = do
  depth <- gen_expr depth a
  genLine "  neg %rax\n"
  return depth

gen_expr depth (Node (UNARY Addr node) _ _) = do
  gen_addr node
  return depth

gen_expr depth (Node (UNARY Deref node) _ _) = do
  depth <- gen_expr depth node
  genLine "  mov (%rax), %rax\n"
  return depth

gen_expr depth (Node (BIN_OP op lhs rhs) _ tok) = do
  depth <- gen_expr depth rhs
  depth <- convertEx $ Right $ push depth
  depth <- gen_expr depth lhs
  depth <- convertEx $ Right $ pop "%rdi" depth
  genLines $gen_bin_op op
  return depth

gen_expr depth (Node _ _ tok) = throwE $ ErrorToken tok "Codegen: invalid expression"

gen_stmt :: Node -> ExceptT CodegenError (State CodegenState) ()
gen_stmt (Node (EXPS_STMT []) _ tok) = return ()

gen_stmt (Node (EXPS_STMT (n:ns)) t tok) = do
  d <- gen_expr 0 n
  let _ = assert (d == 0) 0
  gen_stmt (Node (EXPS_STMT ns) t tok)

gen_stmt (Node (BLOCK nodes) _ tok) = forM_ nodes gen_stmt

gen_stmt (Node (IF cond then_ else_) _ tok) = do
  c <- getCount
  _ <- gen_expr 0 cond

  genLine       "  cmp $0, %rax\n"
  genLine $     "  je  .L.else."++ show c ++"\n"
  gen_stmt then_
  genLine $     "  jmp .L.end." ++ show c ++ "\n"
  genLine $     ".L.else." ++ show c++":\n"

  case else_ of
    Just node -> do
      gen_stmt node
      genLine $ ".L.end."++ show c ++ ":\n"
    Nothing ->
      genLine $ ".L.end."++ show c ++ ":\n"

gen_stmt (Node (FOR ini cond inc body) _ tok) = do
  c <- getCount
  genInit
  genLine $     ".L.begin." ++ show c ++ ":\n"
  genCond c
  gen_stmt body
  genInc
  genLine $ "  jmp .L.begin." ++ show c ++ "\n"
  genLine $ ".L.end." ++ show c ++ ":\n"
  where
    genInit = do
      case ini of
        Nothing -> return ()
        Just node -> do
          gen_stmt node
    genCond c = do
      case cond of
        Nothing -> return ()
        Just node -> do
          _ <- gen_expr 0 node
          genLine $ "  cmp $0, %rax\n"
          genLine $ "  je  .L.end." ++ show c ++"\n"
    genInc = do
      case inc of
        Nothing -> return ()
        Just node -> do
          _ <- gen_expr 0 node
          return ()

gen_stmt (Node (RETURN node) _ tok) = do
  d <- gen_expr 0 node
  let _ = assert (d == 0) 0
  fname <- getCurFuncName
  genLine $ "  jmp .L.return." ++ fname ++ "\n"

gen_stmt (Node _ _ tok) = throwE $ ErrorToken tok ("gen stmt: invalid statement ")

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
assign_lvar_offset (Function node vars _ name args t) = Function node vars' (align_to offset' 16) name args t
  where
    (vars', offset') = f vars 8
    f [] r = ([], r)
    f ((Obj name t _):vs) offset = ((Obj name t (0 - offset)) : vs', offset'') where
      (vs', offset'') = f vs (offset + 8)

gen_block :: [Node] -> ExceptT CodegenError (State CodegenState) ()
gen_block nodes = forM_ nodes gen_stmt

get_obj :: Obj -> [Obj] -> ExceptT CodegenError (State CodegenState) Obj
get_obj (Obj var _ _) locals = do
  case find f locals of
    Nothing ->  throwE $ ErrorText ("variable " ++ var ++ " is not declare")
    Just res -> return res
    where
      f (Obj name _ _) = var == name


codegen_ :: [Function] -> ExceptT CodegenError (State CodegenState) ()
codegen_ = iter where
  iter [] = return ()
  iter (f:fs) = do
    let f' = assign_lvar_offset f
    let locals = functionLocals f'
    let body = functionBody f'
    let stack_size = functionStackSize f'
    let name = functionName f'
    let args = functionArgs f'

    setCurFunc f'

    genLine $ "  .globl " ++ name ++ "\n"
    genLine $ name ++ ":\n"
    -- Prologue
    genLine "  push %rbp\n"
    genLine "  mov %rsp, %rbp\n"
    genLine $ "  sub $" ++ show stack_size ++", %rsp\n"

    --Save passed-by-register arguments to the stack
    add_func_args locals args argreg

    -- Traverse the AST to emit assembly
    gen_block body

    genLine $".L.return." ++ name ++ ":\n"
    genLine "  mov %rbp, %rsp\n"
    genLine "  pop %rbp\n"
    genLine "  ret\n"

    iter fs
  add_func_args _ [] _ = pure ()
  add_func_args locals (a: args) (r:regs)= do
    Obj _ _ offset <- get_obj a locals
    genLine $ "  mov " ++ r ++ ", "++ show offset ++ "(%rbp) \n"
    add_func_args locals args regs

codegen :: [Function] -> Either Error [String]
codegen f = do
  let (r,(code, _, _)) = runState (runExceptT (codegen_ f)) ([], 1, Nothing)
  case r of
    Left e -> Left e
    Right _ -> return code
