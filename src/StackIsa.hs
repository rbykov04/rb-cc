
module StackIsa where

data StackOp
  = PushInt Int
  | ADD
  | SUB
  | MUL
  | DIV
  | CMPEQ    -- ==
  | CMPNE    -- !=
  | CMPLT    -- <
  | CMPLE    -- <=
  | Ret
  deriving (Show, Eq)
