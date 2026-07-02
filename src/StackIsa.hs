
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
  | NEG
  | Ret
  deriving (Show, Eq)
