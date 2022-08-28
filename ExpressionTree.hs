-- an expressiontree evaluation implementation in haskell

module ExpressionTree (evaluate, ExpressionTree(OpNode, ValNode)) where

data ExpressionTree a = OpNode (a -> a -> a) String (ExpressionTree a) (ExpressionTree a) | ValNode a

evaluate :: ExpressionTree a -> a
evaluate (ValNode x) = x
evaluate (OpNode op _ l r) = op (evaluate l) (evaluate r)

instance (Show a) => Show (ExpressionTree a) where
  show (ValNode x) = show x
  show (OpNode _ rep l r) = rep ++ " " ++ show l ++ " " ++ show r
