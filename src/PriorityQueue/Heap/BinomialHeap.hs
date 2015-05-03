module PriorityQueue.Heap.BinomialHeap where

import PriorityQueue.PQ

type BinomialHeap a = [Tree a]

data Ord a => Tree a = EmptyTree | Singleton a | Node a (Tree a) (Tree a) deriving (Show)

merge :: BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge [] [] = []
merge [] l = l
merge l [] = l
