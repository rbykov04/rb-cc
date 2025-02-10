module Codegen where
import RBCC
import Data.List
import Data.Char
import Error
import Control.Monad.Trans.Except
import Control.Monad.State

import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap

type CodegenError = Error
type CodegenState = ([String], Int, Maybe Obj, Int, (IntMap Obj))

getVar :: Int ->ExceptT CodegenError (State CodegenState) Obj
getVar key = do
  (_, _, _, _, storage) <- get
  return $ storage ! key

updateVar :: (Obj -> Obj) -> Int ->ExceptT CodegenError (State CodegenState) ()
updateVar f key = do
  (a, b, c, d, storage) <- get

  let storage' = IntMap.adjust f key storage
  put (a, b, c, d, storage')

getDepth :: ExceptT CodegenError (State CodegenState) Int
getDepth = do
  (_, _, _, depth, _) <- get
  return depth

setDepth :: Int -> ExceptT CodegenError (State CodegenState) ()
setDepth depth = do
  (code, count, f, _, st) <- get
  put (code, count, f, depth, st)

assert_depth_is_0 :: ExceptT CodegenError (State CodegenState) ()
assert_depth_is_0 = do
  d <- getDepth
  unless (d==0) $ throwE $ ErrorText ("depth is not 0")

setCurFunc :: Obj -> ExceptT CodegenError (State CodegenState) ()
setCurFunc func = do
  (code, count, _, depth, s) <- get
  put (code, count, Just func, depth, s)

getCurFunc :: ExceptT CodegenError (State CodegenState) Obj
getCurFunc  = do
  (_, _, myabe_f, _, _) <- get
  case myabe_f of
    Nothing -> throwE $ ErrorText "current func is NULL"
    Just f -> return f

getCurFuncName :: ExceptT CodegenError (State CodegenState) String
getCurFuncName  = fmap objName getCurFunc

getCurLocals :: ExceptT CodegenError (State CodegenState) [Int]
getCurLocals  = fmap objLocals getCurFunc

genLine :: String -> ExceptT CodegenError (State CodegenState) ()
genLine prog = do
  (code, count, f, depth, s) <- get
  put (code ++ [prog], count, f, depth, s)

genLineLn :: String -> ExceptT CodegenError (State CodegenState) ()
genLineLn prog = genLine (prog ++ "\n")

genLines :: [String] -> ExceptT CodegenError (State CodegenState) ()
genLines prog = do
  (code, count, f, depth, s) <- get
  put (code ++ prog, count, f, depth, s)

getCount :: ExceptT CodegenError (State CodegenState) Int
getCount = do
  (code, count, f, depth, s) <- get
  put (code, count + 1, f, depth, s)
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

-- Store %rax to an address that the stack top is pointing to.
store :: Type -> ExceptT CodegenError (State CodegenState) ()
store ty = do
    pop "%rdi"
    if typeSize ty == 1 then
      genLine "  mov %al, (%rdi)\n"
    else
      genLine "  mov %rax, (%rdi)\n"


gen_addr :: Node -> ExceptT CodegenError (State CodegenState) ()
gen_addr (Node kind _ tok) = case kind of
  VAR key -> do
    obj <- getVar key
    let offset = objOffset obj
    if objIsLocal obj then
      genLine $ "  lea " ++ show offset ++ "(%rbp), %rax\n"
    else
      genLine $ "  lea " ++ objName obj ++ "(%rip), %rax\n"

  UNARY Deref node -> do
    setDepth 0
    gen_expr node

  _ -> throwE $ ErrorToken tok "Codegen: not a value"

argreg8 :: [String]
argreg8 = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"]

argreg64 :: [String]
argreg64 = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]

gen_expr ::Node ->ExceptT CodegenError (State CodegenState) ()
gen_expr node@(Node kind ty tok) = case kind of
  VAR _ -> do
    gen_addr node
    load ty

  Assign lhs rhs -> do
    gen_addr lhs
    push
    gen_expr rhs
    store ty

  FUNCALL name arguments -> do
    let nargs = length arguments
    let fregs = reverse $ take nargs argreg64

    forM_ arguments gen_arg
    forM_ fregs pop

    genLineLn $"  mov $0, %rax"
    genLineLn $"  call "++ name

    where
      gen_arg arg = do
        gen_expr arg
        push

  NUM a -> do
    genLineLn $ "  mov $" ++ show a ++ ", %rax"

  UNARY op operand -> case op of
    Neg -> do
      gen_expr operand
      genLineLn "  neg %rax"
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

  STMT_EXPR (Node (BLOCK body) _ _) -> do
    gen_block body

  _ -> throwE $ ErrorToken tok ("Codegen: invalid expression" ++ show (node))

gen_stmt :: Node -> ExceptT CodegenError (State CodegenState) ()
gen_stmt (Node kind _ tok) = case kind of
  EXPS_STMT node -> do
    setDepth 0
    gen_expr node
    --assert_depth_is_0
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
    -- after add gnu stmt_expr - this doesn't work anymore
    -- assert_depth_is_0
    fname <- getCurFuncName
    genLineLn $ "  jmp .L.return." ++ fname

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

assign_lvar_offset :: Int -> ExceptT CodegenError (State CodegenState) ()
assign_lvar_offset key = do
    func <-getVar key
    case (typeKind . objType) func of
      FUNC ret args params _ -> do
        let vars = objLocals func
        vars' <-  mapM getVar vars
        let sizes    = map (typeSize . objType) vars'
        let offsets  = scanl1 (+) sizes
        forM_ (zip vars' offsets) change_offset
        let offset'' = if length vars == 0
                      then 0
                      else ((align_to 16) . last) offsets
        updateVar (change_ftype (Type (FUNC ret args params offset'') 8)) key
      _ -> pure()


change_offset (o,offset) = updateVar (change_offset' offset) (objKey o)
change_offset' offset obj = obj {objOffset= (0 - offset)}
change_ftype t obj = obj {objType = t}

    --f ((Obj name t _):vs) offset = ((Obj name t (0 - offset)) : vs', offset'') where
gen_block :: [Node] -> ExceptT CodegenError (State CodegenState) ()
gen_block nodes = forM_ nodes gen_stmt


emit_data :: Int -> ExceptT CodegenError (State CodegenState) ()
emit_data prog = do
    o <- getVar prog
    let name = objName o
    let size = (typeSize . objType) o
    case (typeKind . objType) o of
      FUNC _ _ _ _ -> pure ()
      _  -> do
        genLineLn $ "  .data"
        genLineLn $ "  .globl " ++ name
        genLineLn $ name ++ ":"
        case objInitData o of
          Nothing   -> genLineLn $ "  .zero " ++ show size
          Just text -> print_bytes text
    where
      print_bytes [] = return ()
      print_bytes (b: bytes) = do
        genLineLn $ "  .byte " ++ ((show . ord) b)
        print_bytes bytes



emit_text :: Int -> ExceptT CodegenError (State CodegenState) ()
emit_text o = do
    f <- getVar o
    case (typeKind . objType) f of
      FUNC _ args _ stack_size -> do
        let body = objBody f
        let name = objName f

        setCurFunc f

        genLineLn $ "  .globl " ++ name
        genLineLn $ "  .text"
        genLineLn $ name ++ ":"
        -- Prologue
        genLineLn "  push %rbp"
        genLineLn "  mov %rsp, %rbp"
        genLineLn $ "  sub $" ++ show stack_size ++", %rsp"

        --Save passed-by-register arguments to the stack
        forM_ (zip3 args argreg8 argreg64) add_func_arg
        -- Traverse the AST to emit assembly
        gen_block body

        genLineLn $ ".L.return." ++ name ++ ":"
        genLineLn   "  mov %rbp, %rsp"
        genLineLn   "  pop %rbp"
        genLineLn   "  ret"
      _  -> pure ()
  where
  add_func_arg (arg, reg8, reg64) = do
    obj <- getVar arg
    let offset = objOffset obj
    if (typeSize .objType) obj  == 1 then
      genLineLn $ "  mov " ++ reg8 ++ ", "++ show offset ++ "(%rbp)"
    else
      genLineLn $ "  mov " ++ reg64 ++ ", "++ show offset ++ "(%rbp)"


codegen_ :: [Int] -> ExceptT CodegenError (State CodegenState) ()
codegen_ vars = do
      mapM_ assign_lvar_offset vars
      mapM_ emit_data vars
      mapM_ emit_text vars

codegen :: [Obj] -> IntMap Obj-> Either Error [String]
codegen f storage= do
  let vars = map objKey f
  let (r,(code, _, _, _, _)) = runState (runExceptT (codegen_ vars)) ([], 1, Nothing, 0, storage)
  case r of
    Left e -> Left e
    Right _ -> return code
