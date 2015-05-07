module PriorityQueue.Heap.BinomialHeap (
    BinomialHeap,
    Tree(EmptyTree, Node),
    mergeTree) where

import PriorityQueue.PQ

import Data.Monoid

type BinomialHeap a = [Tree a]

data Tree a = EmptyTree | Node a (BinomialHeap a) deriving (Show, Eq)

-- instance Ord a => Ord (Tree a) where

instance Ord a => Monoid (Tree a) where
  mempty = EmptyTree
  mappend = mergeTree

mergeTree :: Ord a => Tree a -> Tree a -> Tree a

mergeTree this@(Node thisValue thisHeap) that@(Node thatValue thatHeap) -- merge this into that
  | thisValue < thatValue   = Node thisValue (thisHeap ++ [that])  -- eventually change this to handle min and max heaps
  | otherwise               = Node thatValue (thatHeap ++ [this])

{- mergeTree this@(Node v1 h1) that@(Node v2 h2)
  | v1 < v2 = mergeTree n2 n1
  | otherwise =
-}
