module Codegen where
import RBCC
import Data.List
import Control.Monad.Trans.Except
import Control.Monad.State

type CodegenError = Error
type CodegenState = ([String], Int, Maybe Function, Int)

getDepth :: ExceptT CodegenError (State CodegenState) Int
getDepth = do
  (_, _, _, depth) <- get
  return depth

setDepth :: Int -> ExceptT CodegenError (State CodegenState) ()
setDepth depth = do
  (code, count, f, _) <- get
  put (code, count, f, depth)

setCurFunc :: Function -> ExceptT CodegenError (State CodegenState) ()
setCurFunc func = do
  (code, count, _, depth) <- get
  put (code, count, Just func, depth)

getCurFunc :: ExceptT CodegenError (State CodegenState) Function
getCurFunc  = do
  (_, _, myabe_f, _) <- get
  case myabe_f of
    Nothing -> throwE $ ErrorText "current func is NULL"
    Just f -> return f

getCurFuncName :: ExceptT CodegenError (State CodegenState) String
getCurFuncName  = fmap functionName getCurFunc

getCurLocals :: ExceptT CodegenError (State CodegenState) [Obj]
getCurLocals  = fmap functionLocals getCurFunc

getVariable :: Obj -> ExceptT CodegenError (State CodegenState) Obj
getVariable (Obj var _ _) = do
  locals <- getCurLocals
  case find f locals of
    Nothing ->  throwE $ ErrorText ("variable " ++ var ++ " is not declare")
    Just res -> return res
    where
      f (Obj name _ _) = var == name




genLine :: String -> ExceptT CodegenError (State CodegenState) ()
genLine prog = do
  (code, count, f, depth) <- get
  put (code ++ [prog], count, f, depth)

genLines :: [String] -> ExceptT CodegenError (State CodegenState) ()
genLines prog = do
  (code, count, f, depth) <- get
  put (code ++ prog, count, f, depth)

getCount :: ExceptT CodegenError (State CodegenState) Int
getCount = do
  (code, count, f, depth) <- get
  put (code, count + 1, f, depth)
  return $ count

convertEx :: (Int -> (Int, [String])) -> ExceptT CodegenError (State CodegenState) ()
convertEx f = do
    depth <- getDepth
    let (d, prog) = f depth
    setDepth d
    genLines prog

push_ :: Int -> (Int, [String])
push_ depth = (depth + 1, ["  push %rax\n"])

pop_ :: String -> Int -> (Int, [String])
pop_ text depth = (depth - 1, ["  pop "++text ++ "\n"])

push     = convertEx push_
pop text = convertEx $ pop_ text

gen_addr :: Node -> ExceptT CodegenError (State CodegenState) ()
gen_addr (Node (VAR var) _ _) = do
  Obj _ _ offset <- getVariable var
  genLine $ "  lea " ++ show offset ++ "(%rbp), %rax\n"

gen_addr (Node (UNARY Deref node) _ _) = do
  setDepth 0
  gen_expr node

gen_addr (Node _ _ tok) = throwE $ ErrorToken tok "Codegen: not a value"

argreg :: [String]
argreg = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]

gen_expr ::Node ->ExceptT CodegenError (State CodegenState) ()
gen_expr node@(Node kind _ tok) = case kind of
  VAR _ -> do
    gen_addr node
    genLine "  mov (%rax), %rax\n"

  Assign lhs rhs -> do
    gen_addr lhs
    push
    gen_expr rhs
    pop "%rdi"
    genLine "  mov %rax, (%rdi)\n"

  FUNCALL name arguments -> do
    let nargs = length arguments
    let fregs = reverse $ take nargs argreg

    forM_ arguments gen_arg
    forM_ fregs pop

    genLine $"  mov $0, %rax\n"
    genLine $"  call "++ name ++ "\n"

    where
      gen_arg arg = do
        gen_expr arg
        push

  NUM a -> do
    genLine $ "  mov $" ++ show a ++ ", %rax\n"

  UNARY op operand -> case op of
    Neg -> do
      gen_expr operand
      genLine "  neg %rax\n"
    Addr -> do
      gen_addr operand
    Deref -> do
      gen_expr operand
      genLine "  mov (%rax), %rax\n"

  BIN_OP op lhs rhs -> do
    gen_expr rhs
    push
    gen_expr lhs
    pop "%rdi"
    genLines $gen_bin_op op

  _ -> throwE $ ErrorToken tok "Codegen: invalid expression"

gen_stmt :: Node -> ExceptT CodegenError (State CodegenState) ()

gen_stmt (Node (EXPS_STMT node) t tok) = do
  setDepth 0
  gen_expr node
  d <- getDepth
  let _ = assert (d == 0) 0
  return ()

gen_stmt (Node (BLOCK nodes) _ tok) = forM_ nodes gen_stmt

gen_stmt (Node (IF cond then_ else_) _ tok) = do
  c <- getCount
  setDepth 0
  gen_expr cond

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
          setDepth 0
          gen_expr node
          genLine $ "  cmp $0, %rax\n"
          genLine $ "  je  .L.end." ++ show c ++"\n"
    genInc = do
      case inc of
        Nothing -> return ()
        Just node -> do
          setDepth 0
          gen_expr node
          return ()

gen_stmt (Node (RETURN node) _ tok) = do
  setDepth 0
  gen_expr node
  d <- getDepth
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

codegen_ :: [Function] -> ExceptT CodegenError (State CodegenState) ()
codegen_ = iter where
  iter [] = return ()
  iter (f:fs) = do
    let f' = assign_lvar_offset f
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
    forM_ (zip args argreg) add_func_arg
    -- Traverse the AST to emit assembly
    gen_block body

    genLine $".L.return." ++ name ++ ":\n"
    genLine "  mov %rbp, %rsp\n"
    genLine "  pop %rbp\n"
    genLine "  ret\n"

    iter fs
  add_func_arg (arg, reg) = do
    Obj _ _ offset <- getVariable arg
    genLine $ "  mov " ++ reg ++ ", "++ show offset ++ "(%rbp) \n"

codegen :: [Function] -> Either Error [String]
codegen f = do
  let (r,(code, _, _, _)) = runState (runExceptT (codegen_ f)) ([], 1, Nothing, 0)
  case r of
    Left e -> Left e
    Right _ -> return code
