{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module PriorityQueue.Heap.BinomialHeap (
    BinomialHeap,
    Tree(..),
    mergeTrees) where

import PriorityQueue.PQ

type BinomialHeap a = [Tree a]

data Tree a = Tree { rank :: Int, value :: a, heap :: (BinomialHeap a) } deriving (Show, Eq)

mergeTrees :: (Ord a) => Tree a -> Tree a -> Tree a

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

mergeHeaps :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
mergeHeaps x [] = x
mergeHeaps [] y = y
mergeHeaps (x:xs) (y:ys)
    | rank x < rank y = x : (mergeHeaps xs (y:ys))
    | rank y < rank x = y : (mergeHeaps (x:xs) ys)
    | otherwise = mergeHeaps [mergeTrees x y] (mergeHeaps xs ys)

minVal :: (Ord a) => BinomialHeap a -> a
minVal h = minimum $ map (\ x -> value x) h

popMin :: (Ord a) => BinomialHeap a -> (a, BinomialHeap a)
popMin (x:xs) = bubble x xs
    where
        -- bubble :: Tree a -> BinomialHeap a -> (a, BinomialHeap a)
        bubble m [] = (value m, heap m)
        bubble m (x:xs) =
            let next = bubble newMin xs
                (newMin, skippedTree) =
                    if value x < value m
                    then    (x, m)
                    else    (m, x)
            in (fst next, mergeHeaps [skippedTree] (snd next))
