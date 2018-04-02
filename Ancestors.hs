module Ancestors where

import           Control.Monad.State
import           Data.List

data Tree a
  = Empty
  | Leaf a
  | Node a
         (Tree a)
         (Tree a)
  deriving (Show, Eq)

lowestCommon :: Ord a => a -> a -> Tree a -> a
lowestCommon x y t = minimum $ intersect listA listB
  where
    listA = execState (findAncestors x t) []
    listB = execState (findAncestors y t) []

findAncestors :: Eq a => a -> Tree a -> State [a] Bool
findAncestors _ Empty = return False
findAncestors x (Leaf y)
  | x == y = return True
  | otherwise = return False
findAncestors x (Node y left right)
  | x == y = return True
  | otherwise = do
    left' <- findAncestors x left
    right' <- findAncestors x right
    if left' || right'
      then modify ((:) x) >> return True
      else return False

testTree =
  Node
    6
    (Node 3 (Leaf 17) (Leaf 11))
    (Node 5 (Node 1 (Leaf 11) (Leaf 7)) (Leaf 9))
