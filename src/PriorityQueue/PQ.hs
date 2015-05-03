module PriorityQueue.PQ where

class PQ a where
    add :: k -> a k -> a k
