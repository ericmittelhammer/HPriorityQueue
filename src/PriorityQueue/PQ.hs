{-# LANGUAGE  MultiParamTypeClasses #-}
module PriorityQueue.PQ where

class Ord a => PQ q a where

    enqueue :: q -> a -> q

    dequeue :: q -> (a, q)

    peek :: q -> a

    toList :: q -> [a]
