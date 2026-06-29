
module StackIsa where

data StackOp
  = PushInt Int
  | ADD
  | Ret
  deriving (Show, Eq)
