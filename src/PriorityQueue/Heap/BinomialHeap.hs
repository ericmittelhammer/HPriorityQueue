module PriorityQueue.Heap.BinomialHeap (
    BinomialHeap,
    Tree(..),
    mergeTrees) where

import PriorityQueue.PQ

type BinomialHeap r v = [Tree r v]

data Tree r v = Tree { rank :: r, value :: v, heap :: (BinomialHeap r v) } deriving (Show, Eq)

mergeTrees :: (Num r, Eq r, Ord v) => Tree r v -> Tree r v -> Tree r v

-- merge this into that
mergeTrees this that
    | rank this == rank that =
        Tree (rank hp + 1) (value hp) (heap hp ++ [lp])
    | otherwise = error "Cannot merge trees of different ranks" -- (probably.  might be able to do it recursively w/ mergeHeaps...)
    where
        (hp, lp) = -- hp = "higher priority", lp = "lower priority"
            if value this < value that  -- TODO: support both min and max heaps
                then (this, that)
                else (that, this)

mergeHeaps :: (Num r, Eq r, Ord r, Ord v) => BinomialHeap r v -> BinomialHeap r v -> BinomialHeap r v
mergeHeaps x [] = x
mergeHeaps [] y = y
mergeHeaps (x:xs) (y:ys)
    | rank x < rank y = x : (mergeHeaps xs (y:ys))
    | rank y < rank x = y : (mergeHeaps (x:xs) ys)
    | otherwise = mergeHeaps [mergeTrees x y] (mergeHeaps xs ys)
