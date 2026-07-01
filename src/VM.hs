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
      Ret -> do
        return ()
  where
    binary op = do
        rhs <- pop
        lhs <- pop
        push (lhs `op` rhs)
        eval bytecode


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
