module Main(main) where

import PriorityQueue.Heap.BinomialHeap
--import PriorityQueue.Heap.BinomialHeap( Tree )
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.List

import System.Random

testSingleton = TestCase $ assertEqual
  "Smaller node floats to the top of the tree" (mergeTrees (Tree 0 5 []) (Tree 0 4 [])) (Tree 1 4 [(Tree 0 5 [])])
testPushAndPop = --not really a 'unit' test :(
    let nums = take 10 $ randoms (mkStdGen 9278345) :: [Int]
        sorted = sort nums
        builtHeap = foldl (\ h i -> mergeHeaps h [Tree 0 i []]) [] nums
        asList = toList builtHeap
    in TestCase $ assertEqual
        "Elemets are popped off of the heap in the correct order" sorted asList


tests = hUnitTestToTests $ TestList [ TestLabel "testSingleton" testSingleton, TestLabel "testPushAndPop" testPushAndPop]

main = defaultMain tests
