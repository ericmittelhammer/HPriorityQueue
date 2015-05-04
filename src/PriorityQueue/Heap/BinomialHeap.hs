module PriorityQueue.Heap.BinomialHeap (BinomialHeap, Tree, mergeTree) where

import PriorityQueue.PQ

type BinomialHeap a = [Tree a]

data Ord a => Tree a = EmptyTree | Singleton a | Node a (BinomialHeap a) deriving (Show)

mergeTree :: Ord a => Tree a -> Tree a -> Tree a
mergeTree EmptyTree EmptyTree = EmptyTree
mergeTree EmptyTree h = h
mergeTree h EmptyTree = h
mergeTree h1@(Singleton v1) h2@(Singleton v2)
  | v1 >= v2    = Node v1 [Singleton v2]
  | otherwise   = Node v2 [Singleton v1]
