
module StackIsa where

data StackOp
  = PushInt Int
  | Ret
  deriving (Show, Eq)
