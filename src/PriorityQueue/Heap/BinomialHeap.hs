module PriorityQueue.Heap.BinomialHeap (
    BinomialHeap,
    Tree(..),
    mergeTrees) where

import PriorityQueue.PQ

import Data.Monoid

type BinomialHeap r v = [Tree r v]

data Tree r v = Tree { rank :: r, value :: v, heap :: (BinomialHeap r v) } deriving (Show, Eq)

{-
instance Ord a => Monoid (Tree a) where
  mempty = EmptyTree
  mappend = mergeTrees
-}

{-
instance Ord a => Monoid ([Tree a]) where
  mempty = []
  mappend = mergeHeaps
-}


mergeTrees :: (Num r, Eq r, Ord v) => Tree r v -> Tree r v -> Tree r v

-- merge this into that
mergeTrees this that
    | rank this == rank that =
        Tree (rank smaller + 1) (value smaller) (heap smaller ++ [bigger])
    | otherwise = error "Cannot merge trees of different ranks" -- (probably.  might be able to do it recursively w/ mergeHeaps...)
    where
        (bigger, smaller) =
            if value this > value that
                then (this, that)
                else (that, this)

mergeHeaps :: (Num r, Eq r, Ord r, Ord v) => BinomialHeap r v -> BinomialHeap r v -> BinomialHeap r v
mergeHeaps x [] = x
mergeHeaps [] y = y
mergeHeaps (x:xs) (y:ys)
    | rank x < rank y = x : (mergeHeaps xs (y:ys))
    | rank y < rank x = y : (mergeHeaps (x:xs) ys)
    | otherwise = mergeHeaps (mergeTrees x y : xs) ys


{- merge (x:[]) (y:[]) = [x <> y]
merge (x:[]) (y:ys) = x <> y : ys
merge (x:xs) (y:[]) = x <> y : xs -}

{- insert el (EmptyTree:xs) = (Node el []):xs
insert el (x:xs) = (mergeTree (Node))
-}
{- mergeTree this@(Node v1 h1) that@(Node v2 h2)
  | v1 < v2 = mergeTree n2 n1
  | otherwise =
-}
