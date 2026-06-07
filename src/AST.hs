{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AST where
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.IntMap.Lazy (IntMap, (!))
import qualified Data.IntMap.Lazy as IntMap
import Tokenize
import Error

--Phases
data Parsed
data Typed

type family XVar p
type instance XVar Parsed = String
type instance XVar Typed  = Int

type family XNodeExt p
type instance XNodeExt Parsed = Token
type instance XNodeExt Typed  = (Token, Type)

nodeType :: Node Typed -> Type
nodeType node = snd (nodeExt node)

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

data Node_ p =
  NUM Int
  | VAR (XVar p)
  | Assign (Node p) (Node p)
  | BIN_OP BinOp (Node p) (Node p)
  | UNARY UnOp (Node p)
  | EXPS_STMT (Node p)
  | STMT_EXPR (Node p)
--          name    args
  | FUNCALL String [(Node p)]
  | RETURN (Node p)
  | BLOCK [(Node p)]

-- "if" statement
--  IF cond then else
  | IF (Node p) (Node p) (Maybe (Node p))

-- "for" or "while" statement
--  FOR init          cond        inc          body
  | FOR (Maybe (Node p)) (Maybe (Node p)) (Maybe (Node p)) (Node p)

data Node p = Node
  {
    nodeNode  :: (Node_ p),
    nodeExt  :: XNodeExt p
  }

deriving instance (Show (XVar p), Show (XNodeExt p)) => Show (Node_ p)
deriving instance (Eq (XVar p),   Eq (XNodeExt p))   => Eq (Node_ p)

deriving instance (Show (XVar p), Show (XNodeExt p)) => Show (Node p)
deriving instance (Eq (XVar p),   Eq (XNodeExt p))   => Eq (Node p)


data Obj = Obj
  {
    objKey        :: Int,
    objName       :: String, -- variable name
    objType       :: Type  , -- type
    objOffset     :: Int   , -- offset from RBP
    objIsLocal    :: Bool  , -- local or global/function
    objBody       :: [Node Typed],
    objLocals     :: [Int],
    objInitData   :: Maybe String -- global_variable
  }
  deriving (Show, Eq)


data VarScope = VarScope
  {
    scopeName :: String,
    scopeObj  :: Obj
  }
  deriving (Show, Eq)

data Scope = Scope
  {
    scopeVars :: [VarScope]
  }
  deriving (Show, Eq)

--tmp
data ParserState = ParserState
  { tokens      :: [Token]
  , currentVars :: ([Obj], [Obj])
  , allObjects  :: IntMap Obj
  , uniqCounter :: Int
  , scopes      :: [Scope]
  } deriving (Show)
