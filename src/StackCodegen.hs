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
  BIN_OP op lhs rhs  ->
    let
        visitedLhs = go lhs
        visitedRhs = go rhs
        binOp = case op of
          Add -> [ADD]
          _ -> error $ "not implemented yet" ++ show op
    in visitedLhs ++ visitedRhs ++ binOp
  _ -> error $ "not implemented yet" ++ show node
  where go = visitNode
