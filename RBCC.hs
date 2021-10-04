module RBCC where
--
-- Tokenizer
--

data TokenKind =
  EOF
  | Ident String
  | Punct String
  | Num Int
  deriving (Eq, Show)

data Token = Token
  {
    tokenKind :: TokenKind,
    tokenLen :: Int,
    tokenLoc :: Int
  }
  deriving (Eq, Show)

--
-- Parser
--

data Error = ErrorCode Int | ErrorText String | ErrorToken Token String deriving Show
data BinOp =
  Add
  | Sub
  | Mul
  | Div
  | ND_EQ
  | ND_NE
  | ND_LT
  | ND_LE
  | Assign
  deriving (Show, Eq)
data UnOp = Neg deriving (Show, Eq)

data Node =
  NUM Int
  | VAR String
  | BIN_OP BinOp Node Node
  | UNARY UnOp Node
  | EXPS_STMT [Node]
  deriving (Show, Eq)
