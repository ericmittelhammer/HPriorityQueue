module PriorityQueue.Heap.BinomialHeap (
    BinomialHeap,
    Tree(EmptyTree, Singleton, Node),
    mergeTree) where

import PriorityQueue.PQ

import Data.Monoid

type BinomialHeap a = [Tree a]

data Tree a = EmptyTree | Singleton a | Node a (BinomialHeap a) deriving (Show, Eq)

-- instance Ord a => Ord (Tree a) where

instance Ord a => Monoid (Tree a) where
  mempty = EmptyTree
  mappend = mergeTree

mergeTree :: Ord a => Tree a -> Tree a -> Tree a

mergeTree (Singleton v1) (Singleton v2)
  | v1 < v2    = Node v1 [Singleton v2]  -- eventually change this to handle min and max heaps
  | otherwise   = Node v2 [Singleton v1]

{- mergeTree this@(Node v1 h1) that@(Node v2 h2)
  | v1 < v2 = mergeTree n2 n1
  | otherwise =
-}
