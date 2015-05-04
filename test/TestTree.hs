module Main(main) where

import PriorityQueue.Heap.BinomialHeap
import PriorityQueue.Heap.BinomialHeap( Tree(EmptyTree, Singleton, Node) )
import Test.HUnit

testEmpty = TestCase $ assertEqual
  "Larger node floats to the top of the tree" (mergeTree (Singleton 5) (Singleton 4)) (Node 5 [(Singleton 4)])
-- testNegCursor = TestCase $ assertEqual
--   "Should get Nothing when cursor is negative" Nothing ( findIdentifier "a" (-1, -1) )
-- testComment = TestCase $ assertEqual
--   "Should get Nothing on comment" Nothing ( findIdentifier "-- a" (1, 3) )
-- testMinimal = TestCase $ assertEqual
--   "Minimal program" (Just "main") ( findIdentifier "main = print 42" (1, 2) )

main = runTestTT $ TestList [testEmpty] --, testNegCursor, testComment, testMinimal]
