module Main(main) where

import PriorityQueue.Heap.BinomialHeap
import PriorityQueue.Heap.BinomialHeap( Tree(EmptyTree, Node) )
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

testSingleton = TestCase $ assertEqual
  "Smaller node floats to the top of the tree" (mergeTree (Node 5 []) (Node 4 [])) (Node 4 [(Node 5 [])])
-- testNegCursor = TestCase $ assertEqual
--   "Should get Nothing when cursor is negative" Nothing ( findIdentifier "a" (-1, -1) )
-- testComment = TestCase $ assertEqual
--   "Should get Nothing on comment" Nothing ( findIdentifier "-- a" (1, 3) )
-- testMinimal = TestCase $ assertEqual
--   "Minimal program" (Just "main") ( findIdentifier "main = print 42" (1, 2) )

tests = hUnitTestToTests $ TestList [TestLabel "testSingleton" testSingleton]

main = defaultMain tests
