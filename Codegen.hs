module Codegen where
import RBCC
import Data.List
import Control.Monad.Trans.Except
import Control.Monad.State

type CodegenError = Error
type CodegenState = ([String], Int, Maybe Obj, Int)

getDepth :: ExceptT CodegenError (State CodegenState) Int
getDepth = do
  (_, _, _, depth) <- get
  return depth

setDepth :: Int -> ExceptT CodegenError (State CodegenState) ()
setDepth depth = do
  (code, count, f, _) <- get
  put (code, count, f, depth)

assert_depth_is_0 :: ExceptT CodegenError (State CodegenState) ()
assert_depth_is_0 = do
  d <- getDepth
  unless (d==0) $ throwE $ ErrorText ("depth is not 0")

setCurFunc :: Obj -> ExceptT CodegenError (State CodegenState) ()
setCurFunc func = do
  (code, count, _, depth) <- get
  put (code, count, Just func, depth)

getCurFunc :: ExceptT CodegenError (State CodegenState) Obj
getCurFunc  = do
  (_, _, myabe_f, _) <- get
  case myabe_f of
    Nothing -> throwE $ ErrorText "current func is NULL"
    Just f -> return f

getCurFuncName :: ExceptT CodegenError (State CodegenState) String
getCurFuncName  = fmap objName getCurFunc

getCurLocals :: ExceptT CodegenError (State CodegenState) [Obj]
getCurLocals  = fmap objLocals getCurFunc

getVariable :: Obj -> ExceptT CodegenError (State CodegenState) Obj
getVariable var = do
  locals <- getCurLocals
  case find f locals of
    Nothing ->  throwE $ ErrorText ("variable " ++ objName var ++ " is not declare")
    Just res -> return res
    where
      f o = objName var == objName o


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


-- Load a value from where %rax is pointing to.
load :: Type -> ExceptT CodegenError (State CodegenState) ()
load ty = case typeKind ty of
    -- If it is an array, do not attempt to load a value to the
    -- register because in general we can't load an entire array to a
    -- register. As a result, the result of an evaluation of an array
    -- becomes not the array itself but the address of the array.
    -- This is where "array is automatically converted to a pointer to
    -- the first element of the array in C" occurs.
    ARRAY _ _ -> return ()
    _ -> do
      genLine "  mov (%rax), %rax\n"

store :: ExceptT CodegenError (State CodegenState) ()
store = do
    pop "%rdi"
    genLine "  mov %rax, (%rdi)\n"


gen_addr :: Node -> ExceptT CodegenError (State CodegenState) ()
gen_addr (Node kind _ tok) = case kind of
  VAR var -> do
    obj <- getVariable var
    let offset = objOffset obj
    genLine $ "  lea " ++ show offset ++ "(%rbp), %rax\n"

  UNARY Deref node -> do
    setDepth 0
    gen_expr node

  _ -> throwE $ ErrorToken tok "Codegen: not a value"

argreg :: [String]
argreg = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]

gen_expr ::Node ->ExceptT CodegenError (State CodegenState) ()
gen_expr node@(Node kind ty tok) = case kind of
  VAR _ -> do
    gen_addr node
    load ty

  Assign lhs rhs -> do
    gen_addr lhs
    push
    gen_expr rhs
    store

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
      load ty

  BIN_OP op lhs rhs -> do
    gen_expr rhs
    push
    gen_expr lhs
    pop "%rdi"
    genLines $gen_bin_op op

  _ -> throwE $ ErrorToken tok "Codegen: invalid expression"

gen_stmt :: Node -> ExceptT CodegenError (State CodegenState) ()
gen_stmt (Node kind _ tok) = case kind of
  EXPS_STMT node -> do
    setDepth 0
    gen_expr node
    assert_depth_is_0
    return ()

  BLOCK nodes -> forM_ nodes gen_stmt

  IF cond then_ else_ -> do
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

  FOR ini cond inc body -> do
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

  RETURN node -> do
    setDepth 0
    gen_expr node
    assert_depth_is_0
    fname <- getCurFuncName
    genLine $ "  jmp .L.return." ++ fname ++ "\n"

  _ -> throwE $ ErrorToken tok ("gen stmt: invalid statement ")


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


-- Round up `n` to the nearest multiple of `align`. For instance,
-- align_to 5 8  returns 8 and align_to 11  8  returns 16.
align_to :: Int -> Int -> Int
align_to align n = ((n + align - 1) `div` align) * align

assign_lvar_offset :: Obj -> Obj
assign_lvar_offset obj = obj {objLocals = vars',  objOffset = offset''}
  where
    vars     = objLocals obj
    sizes    = map (typeSize . objType) vars
    offsets  = scanl1 (+) sizes
    vars'    = map (uncurry change_offset) $ zip vars offsets
    offset'' = if length vars' == 0
                  then 0
                  else ((align_to 16) . last) offsets


change_offset obj offset = obj {objOffset= (0 - offset)}

    --f ((Obj name t _):vs) offset = ((Obj name t (0 - offset)) : vs', offset'') where
gen_block :: [Node] -> ExceptT CodegenError (State CodegenState) ()
gen_block nodes = forM_ nodes gen_stmt

codegen_ :: [Obj] -> ExceptT CodegenError (State CodegenState) ()
codegen_ = iter where
  iter [] = return ()
  iter (f:fs) = do
    let f' = assign_lvar_offset f
    let body = objBody f'
    let stack_size = objStackSize f'
    let name = objName f'
    let args = objArgs f'

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
    obj <- getVariable arg
    let offset = objOffset obj
    genLine $ "  mov " ++ reg ++ ", "++ show offset ++ "(%rbp) \n"

codegen :: [Obj] -> Either Error [String]
codegen f = do
  let (r,(code, _, _, _)) = runState (runExceptT (codegen_ f)) ([], 1, Nothing, 0)
  case r of
    Left e -> Left e
    Right _ -> return code
