module PriorityQueue.Heap.BinaryHeap where

import PriorityQueue.PQ

data Ord a => Tree a = EmptyTree | Singleton a | Node a (Tree a) (Tree a) deriving (Show)
