{-
It is a simple structure to save context for IR
-}

module Context where
import Tokenize


data Context = Context Token
                | NullCtx String
  deriving (Show, Eq)

mkCtx :: Token -> Context
mkCtx = Context

mkNullCtx = NullCtx ""
mkNullCtxText s = NullCtx s
