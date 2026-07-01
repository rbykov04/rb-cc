
module StackIsa where

data StackOp
  = PushInt Int
  | ADD
  | SUB
  | MUL
  | DIV
  | Ret
  deriving (Show, Eq)
