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

--Phases
data Parsed
data Typed

type family XVar p
type instance XVar Parsed = String
type instance XVar Typed  = Int

type family XNodeExt p
type instance XNodeExt Parsed = Token
type instance XNodeExt Typed  = (Token, Type Typed)

nodeType :: Node Typed -> Type Typed
nodeType node = snd (nodeExt node)

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



type family XFuncExt p
type instance XFuncExt Parsed = [(Type Parsed, String)]
type instance XFuncExt Typed  = ([Int] , [Int], Int)

data TypeKind p
  = INT
  | CHAR
  | PTR (Type p)
  -- arr  base len
  | ARRAY (Type p) Int
--       typy args locals
  | FUNC (Type p) (XFuncExt p)
  --  FUNC Type [(Type, String)]

-- Pointer

-- Pointer-to or array-of type. We intentionally use the same member
-- to represent pointer/array duality in C.

-- In many contexts in which a pointer is expected, we examine this
-- member instead of "kind" member to determine whether a type is a
-- pointer or not. That means in many contexts "array of T" is
-- naturally handled as if it were "pointer to T", as required by
-- the C spec.

data Type p = Type
  {
    typeKind :: TypeKind p,
    typeSize :: Int -- sizeof() value --FIXME: for PARSED it is useless
  }


data ParserOnlyNodes
  = DECL_VAR String (Type Parsed)
  | FUNCTION String (Type Parsed) (Node Parsed)
  | STR_VALUE String

type family XP p
type instance XP Parsed = ParserOnlyNodes
type instance XP Typed = ()

data Node_ p =
  NUM Int
  | VAR (XVar p)
  | Assign (Node p) (Node p)
  | BIN_OP BinOp (Node p) (Node p)
  | SIZEOF (Node p)
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
  | EXT (XP p)

data Node p = Node
  {
    nodeNode  :: (Node_ p),
    nodeExt  :: XNodeExt p
  }



data Obj = Obj
  {
    objKey        :: Int,
    objName       :: String, -- variable name
    objType       :: Type Typed , -- type
    objOffset     :: Int   , -- offset from RBP
    objIsLocal    :: Bool  , -- local or global/function
    objBody       :: [Node Typed],
    objLocals     :: [Int],
    objInitData   :: Maybe String -- global_variable
  }

deriving instance (Show (XFuncExt p), Show (Type p)) => Show (TypeKind p)
deriving instance (Eq (XFuncExt p), Eq (Type p)) => Eq (TypeKind p)

deriving instance Show (TypeKind p) => Show (Type p)
deriving instance Eq (TypeKind p) => Eq (Type p)


deriving instance (Eq (XVar p), Eq (XP p), Eq (XP p), Eq (XNodeExt p)) => Eq (Node_ p)
deriving instance (Show (XVar p), Show (XP p), Show (XP p), Show (XNodeExt p)) => Show (Node_ p)

deriving instance (Eq (XVar p), Eq (XP p), Eq (XP p), Eq (XNodeExt p)) => Eq (Node p)
deriving instance (Show (XVar p), Show (XP p), Show (XP p), Show (XNodeExt p)) => Show (Node p)

deriving instance Show ParserOnlyNodes
deriving instance Eq ParserOnlyNodes


deriving instance Eq Obj
deriving instance Show Obj


data VarScope = VarScope
  {
    scopeName :: String,
    scopeObj  :: Obj
  } deriving (Show)

data Scope = Scope
  {
    scopeVars :: [VarScope]
  } deriving (Show)

