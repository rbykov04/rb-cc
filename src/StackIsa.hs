
module StackIsa where

data StackOp
  = PushInt Int
  | ADD
  | SUB
  | Ret
  deriving (Show, Eq)
