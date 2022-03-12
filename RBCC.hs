module RBCC where
--
-- Tokenizer
--

data TokenKind =
  EOF
  | Ident String   -- Identifiers
  | Punct String   -- Punctuators
  | Num Int        -- Numeric Literals
  | Keyword String -- Keywords
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
  | Sub      -- -
  | Mul      -- *
  | Div      -- /
  | ND_EQ    -- ==
  | ND_NE    -- !=
  | ND_LT    -- <
  | ND_LE    -- <=
  | Assign   -- =
  deriving (Show, Eq)

data UnOp =
  Neg        -- unary -
  | Addr     -- unary &
  | Deref    -- unary *
   deriving (Show, Eq)

data Type
  = INT
  | PTR Type
  deriving (Show, Eq)

data Node_ =
  NUM Int
  | VAR Obj
  | BIN_OP BinOp Node Node
  | UNARY UnOp Node
  | EXPS_STMT [Node]
--          name    args
  | FUNCALL String [Node]
  | RETURN Node            -- "return"
  | BLOCK [Node]

-- "if" statement
--  IF cond then else
  | IF Node Node (Maybe Node)

-- "for" or "while" statement
--  FOR init          cond        inc          body
  | FOR (Maybe Node) (Maybe Node) (Maybe Node) Node
  deriving (Show, Eq)

data Node = Node Node_ Type Token deriving (Show, Eq)

data Obj = Obj
    String -- variable name
    Type   -- type
    Int    -- offset from RBP
    deriving (Show, Eq)

data Function = Function {
    functionBody      :: [Node],
    functionLocals    :: [Obj],
    functionStackSize :: Int
  }
  deriving (Show, Eq)
