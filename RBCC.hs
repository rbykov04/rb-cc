module RBCC where
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap

--
-- Tokenizer
--

data TokenKind =
  EOF
  | Ident String   -- Identifiers
  | Str String     -- String literals
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
  | CHAR
  | PTR Type
  -- arr  base len
  | ARRAY Type Int
--       typy args locals
  | FUNC Type [Int] [Int] Int
  --  FUNC Type [(Type, String)]
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
  | VAR Int
  | Assign Node Node  -- =
  | BIN_OP BinOp Node Node
  | UNARY UnOp Node
  | EXPS_STMT Node
  | STMT_EXPR Node
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
    objKey        :: Int,
    objName       :: String, -- variable name
    objType       :: Type  , -- type
    objOffset     :: Int   , -- offset from RBP
    objIsLocal    :: Bool  , -- local or global/function
    objBody       :: [Node],
    objLocals     :: [Int],
    objInitData   :: Maybe String -- global_variable
  }
  deriving (Show, Eq)


type Vars = ([Obj], [Obj])
type ParserState = ([Token], Vars, IntMap Obj, Int)

fst'   (a, _, _, _) = a
snd'   (_, a, _, _) = a
thrd'  (_, _, a, _) = a
forth' (_, _, _, a) = a


getVars :: ExceptT Error (State ParserState) (IntMap Obj)
getVars = do
  r <- get
  return $ thrd' r


get_var :: Int -> ExceptT Error (State ParserState) Obj
get_var key = do
  vars <- getVars
  return $ vars ! key
