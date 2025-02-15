module CInterpreter where

import Parse2

newtype Memory   = Memory [(String, Int)] deriving (Show, Eq)
lookupMem :: Memory -> String -> Maybe Int
lookupMem (Memory mem) name = lookup' mem name
        where lookup' [] _ = Nothing
              lookup' ((x_name, x_value): xs) name =
                if name == x_name
                then Just x_value
                else lookup' xs name


data Op    = Get String
        | Put String Int
        | Copy String String
        | Declare String
        deriving (Show, Eq)

newtype ControlFlow = List Op        deriving (Show, Eq)
data    Machine     = Machine Memory deriving (Show, Eq)

data ErrorExec = Redeclaration deriving (Show, Eq)

convert'' :: VarDecl -> Op
convert'' (Variable ctx name ty) = Declare name
convert'' (Pointer  ctx name ty) = Declare name

convert' :: Decl -> [Op]
convert' (VariableDecl _ _ vars)  = map convert'' vars
convert' _  = []




convertSt :: Stmt -> [Op]
convertSt (Block _ body) = concatMap convertSt body
convertSt (Stmt_Expr _ (ASSIGN _ (Prim (Var _ name)) (Prim src))) =
    case src of
      Var _ x ->  [Copy name x]
      Number _ x -> [Put name x]
convertSt _ = []


convertf (Function _ _ name stmss)  = [(name, concatMap convertSt stmss)]
convertf _  = []


convert (Program x) = concatMap convert' x

getFunctions (Program x) = concatMap convertf x

eval :: Memory -> Op -> Either ErrorExec Memory
-- what about undefined value here? FIXME
eval memory@(Memory mem) (Declare name) = case lookupMem memory name of
  Just _ -> Left Redeclaration
  _ -> Right $ Memory ((name, 0) : mem)
eval mem _ = Right mem

trace :: Memory -> [Op] -> Either ErrorExec (Memory, [String])
trace mem [] = return (mem, [])
trace mem (op : ops) = do
        memory <- eval mem op
        let t =  show op ++ " -> " ++  show memory
        (memory2, ts) <- trace memory ops
        return (memory2, t: ts)

tryTrace mem cf = case trace mem cf of
  Left e -> ["Error " ++ show e]
  Right (m, t) -> t --FIXME add trace even if error

runMachine :: Program -> [String]
runMachine  prog = let
  control_flow = convert prog

  funcs = getFunctions prog
  init = Machine (Memory [])
  trs = tryTrace (Memory []) control_flow
  in
        ["Funcs"]
        ++ [show funcs]
        ++ ["Null State"]
        ++ [show init]
        ++ [show control_flow]
        ++ trs
        ++ ["End State"]
