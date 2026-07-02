module VM where

import Control.Monad.State
import StackIsa

--import Data.IntMap.Lazy (IntMap, (!))

type RetVal = Int
--type VmMemory = IntMap Int

data VMState = VMState
  { vmStack :: [Int]
  , vmPC    :: Int
  } deriving (Show)

type Interpreter a = State VMState a

runVM :: [StackOp] -> RetVal
runVM code =
  let init = VMState [] 0
      finalState = execState (eval code)  init
  in case vmStack finalState of
    (top : _) -> top
    []        -> 0

eval :: [StackOp] -> Interpreter ()
eval bytecode = do
  pc <- gets  vmPC
  if pc >= length bytecode
  then return ()
  else do
    let opcode = bytecode !! pc
    modify (\s -> s {vmPC = pc + 1})
    case opcode of
      PushInt n -> do
        modify (\s -> s {vmStack = n : vmStack s})
        eval bytecode
      ADD -> binary (+)
      SUB -> binary (-)
      MUL -> binary (*)
      DIV -> binary div
      CMPEQ -> binary (bool2Int (==))
      CMPNE -> binary (bool2Int (/=))
      CMPLT -> binary (bool2Int (<))
      CMPLE -> binary (bool2Int (<=))
      NEG -> do
        a <- pop
        push (- a)
        eval bytecode

      Ret -> do
        return ()
  where
    binary op = do
        rhs <- pop
        lhs <- pop
        push (lhs `op` rhs)
        eval bytecode

    bool2Int op l r = if l `op` r then 1 else 0


    pop = do
      s <- get
      let (n, newStack) = case vmStack s of
                []     -> error "we can't pop empty stack"
                (h: t) -> (h , t)
      modify (\s -> s {vmStack = newStack})
      return n
    push n = do
        modify (\s -> s {vmStack = n : vmStack s})

gen :: [StackOp] -> [Int]
gen [] = []
gen (PushInt n : xs) = 0 : n : gen xs
gen (Ret       : xs) = 1 : gen xs
gen (ADD       : xs) = 2 : gen xs
gen (SUB       : xs) = 3 : gen xs
gen (MUL       : xs) = 4 : gen xs
gen (DIV       : xs) = 5 : gen xs
gen (CMPEQ     : xs) = 6 : gen xs
gen (CMPNE     : xs) = 7 : gen xs
gen (CMPLT     : xs) = 8 : gen xs
gen (CMPLE     : xs) = 9 : gen xs
gen (NEG       : xs) = 10 : gen xs
