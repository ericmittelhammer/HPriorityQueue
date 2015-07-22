module PriorityQueue.Heap.BinomialHeap (
    BinomialHeap,
    Tree(..),
    mergeTrees,
    mergeHeaps,
    toList) where

import PriorityQueue.PQ
import Control.Monad.State

type BinomialHeap a = [Tree a]

data Tree a = Tree {
    rank :: Int,
    value :: a,
    heap :: BinomialHeap a
} deriving (Show, Eq)

mergeTrees :: (Ord a) => Tree a -> Tree a -> Tree a

mergeTrees t1 t2
    | rank t1 == rank t2    = Tree (rank hp + 1) (value hp) (heap hp ++ [lp])
    | otherwise                 = error "Cannot merge trees of different ranks" -- (probably.  might be able to do it recursively w/ mergeHeaps...)
    where   -- hp = "higher priority", lp = "lower priority"
        (hp, lp) = if value t1 < value t2  -- TODO: support both min and max heaps
                    then (t1, t2)
                    else (t2, t1)

mergeHeaps :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
mergeHeaps x [] = x
mergeHeaps [] y = y
mergeHeaps (x:xs) (y:ys)
    | rank x < rank y   = x : mergeHeaps xs (y:ys)
    | rank y < rank x   = y : mergeHeaps (x:xs) ys
    | otherwise         = mergeHeaps [mergeTrees x y] (mergeHeaps xs ys)

minVal :: (Ord a) => BinomialHeap a -> a
minVal h = minimum $ map value h



bubble :: (Ord a) => BinomialHeap a -> State a (BinomialHeap a)
bubble (x:[]) = do
  put $ value x
  return $ heap x
bubble (f:s:xs) = do
  put $ minimum [value f, value s]
  let (hp, lp) = if value f < value s  -- TODO: support both min and max heaps
              then (f, s)
              else (s, f)
  rest <- bubble (hp:xs)
  return $ mergeHeaps [lp] rest


popMin :: (Ord a) => BinomialHeap a -> (BinomialHeap a, a)
popMin (x:xs) = runState (bubble (x:xs)) (value x)

push :: (Ord a) => BinomialHeap a -> a -> BinomialHeap a
push heap val = mergeHeaps heap ([Tree {rank=0, value=val, heap=[]}])

toList :: (Ord a) => BinomialHeap a -> [a]
toList [] = []
toList h = popped : toList remainingHeap
    where (remainingHeap, popped) = popMin h
