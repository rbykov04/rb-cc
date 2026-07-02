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
  UNARY op n  ->
    let
        visitedN = go n
        unOp = case op of
          Neg -> [NEG]
          _ -> error $ "not implemented yet" ++ show node
    in visitedN ++ unOp
  BIN_OP op lhs rhs  ->
    let
        visitedLhs = go lhs
        visitedRhs = go rhs
        binOp = case op of
          Add -> [ADD]
          Sub -> [SUB]
          Mul -> [MUL]
          Div -> [DIV]
          ND_EQ -> [CMPEQ]
          ND_NE -> [CMPNE]
          ND_LT -> [CMPLT]
          ND_LE -> [CMPLE]
    in visitedLhs ++ visitedRhs ++ binOp
  _ -> error $ "not implemented yet" ++ show node
  where go = visitNode
