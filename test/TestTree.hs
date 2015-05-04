module TestTree where

import PriorityQueue.Heap.BinomialHeap
import Test.HUnit

testEmpty = TestCase $ assertEqual
  "Should get Nothing from an empty string" 1 (1)
-- testNegCursor = TestCase $ assertEqual
--   "Should get Nothing when cursor is negative" Nothing ( findIdentifier "a" (-1, -1) )
-- testComment = TestCase $ assertEqual
--   "Should get Nothing on comment" Nothing ( findIdentifier "-- a" (1, 3) )
-- testMinimal = TestCase $ assertEqual
--   "Minimal program" (Just "main") ( findIdentifier "main = print 42" (1, 2) )

main = runTestTT $ TestList [testEmpty] --, testNegCursor, testComment, testMinimal]
