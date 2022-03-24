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
  deriving (Show, Eq)

data UnOp =
  Neg        -- unary -
  | Addr     -- unary &
  | Deref    -- unary *
   deriving (Show, Eq)

--
data TypeKind
  = INT
  | PTR Type
  -- arr  base len
  | ARRAY Type Int
--       typy args
  | FUNC Type [(Type, String)]
  deriving (Show, Eq)

-- Pointer

-- Pointer-to or array-of type. We intentionally use the same member
-- to represent pointer/array duality in C.

-- In many contexts in which a pointer is expected, we examine this
-- member instead of "kind" member to determine whether a type is a
-- pointer or not. That means in many contexts "array of T" is
-- naturally handled as if it were "pointer to T", as required by
-- the C spec.

data Type = Type
  {
    typeKind :: TypeKind,
    typeSize :: Int -- sizeof() value

  }
  deriving (Show, Eq)

data Node_ =
  NUM Int
  | VAR Obj
  | Assign Node Node  -- =
  | BIN_OP BinOp Node Node
  | UNARY UnOp Node
  | EXPS_STMT Node
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

data Node = Node
  {
    nodeNode  :: Node_,
    nodeType  :: Type,
    nodeToken :: Token
  }
  deriving (Show, Eq)

data Obj = Obj
  {
    objName       :: String, -- variable name
    objType       :: Type  , -- type
    objOffset     :: Int   , -- offset from RBP
    objIsLocal    :: Bool  , -- local or global/function
    objIsFunction :: Bool  , -- global or function
    objBody       :: [Node],
    objLocals     :: [Obj] ,
    objStackSize  :: Int,
    objArgs       :: [Obj]
  }
  deriving (Show, Eq)

