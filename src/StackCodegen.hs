module StackCodegen (toIR) where

import AST
import StackIsa

toIR :: Node Typed -> [StackOp]
toIR node = visitNode node

visitNode :: Node Typed ->  [StackOp]
visitNode node = case nodeNode node of
  NUM n    -> [PushInt n]
  RETURN v -> go v ++ [Ret]
  BLOCK nodes -> concatMap go nodes
  _ -> error "not implemented yet"
  where go = visitNode
