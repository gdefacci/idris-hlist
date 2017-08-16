
module Test.HList

import Data.Vect
import Data.HList

%access public export
%default total

assertEq : Eq a => String -> (given : a) -> (expected : a) -> IO ()
assertEq desc g e = if g == e
    then putStr $ ""
    else putStrLn $ "Test Failed " ++ desc

sample3 : HList 3 [Nat, Bool, String]
sample3 = [S Z, True, "Yeah"]

sample3a : HList 3 [Bool, String, Integer]
sample3a = [True, "Yeah", 10]

sample4 : HList 4 [Bool, String, Integer, Nat]
sample4 = [False, "Text", 10, 4]

sample4a : HList 4 [Bool, String, Integer, Nat]
sample4a = [False, "Text", 11, 4]


testLength :  IO ()
testLength = do
  assertEq "length" (length sample3) 3
  assertEq "length" (length sample4) 4

testGet :  IO ()
testGet =
  do
    assertEq "TestGet" (S Z) (get 0 sample3)
    assertEq "TestGet" True (get 1 sample3)
    assertEq "TestGet" "Yeah" (get 2 sample3)

testSet : IO ()
testSet = do
  assertEq "TestSet" [Z, True, "Yeah"] $ set 0 sample3 Z
  assertEq "TestSet" [S Z, False, "Yeah"] $ set 1 sample3 False

testHead : IO ()
testHead = assertEq "TestHead" (S Z) $ head sample3

testLast : IO ()
testLast = assertEq "TestLast" "Yeah" $ last sample3

testZip : IO ()
testZip =
  assertEq "TestZip" [(S Z, True), (True, "Yeah"), ("Yeah", 10)] $ zip sample3 sample3a

testTake : IO ()
testTake =
  assertEq "TestTake" [S Z, True] $ take 2 sample3

testDrop : IO ()
testDrop =
  assertEq "TestDrop" [True, "Yeah"] $ drop 1 sample3

testInit : IO ()
testInit = do
  assertEq "TestInit" [False, "Text", 10] $ init sample4
  assertEq "TestInit" [S Z, True] $ init sample3

testTail : IO ()
testTail = do
  assertEq "TestTail" [True, "Yeah"] $ tail sample3
  assertEq "TestTail" ["Text", 10, 4] $ tail sample4

testConcat : IO ()
testConcat = do
    assertEq "TestConcat" [S Z, True, "Yeah", False, "Text", 10, 4] $ sample3 ++ sample4
    assertEq "TestConcat" [False, "Text", 10, 4, S Z, True, "Yeah"] $ sample4 ++ sample3

testUpdate : IO ()
testUpdate = do
  assertEq "TestUpdate" ["YS", "Text", 10, 4] $ update 0 sample4 "YS"
  assertEq "TestUpdate" [False, "Text", "YS", 4] $ update 2 sample4 "YS"
  assertEq "TestUpdate" [False, "Text", 10, "YS"] $ update 3 sample4 "YS"


testInsertAt : IO ()
testInsertAt = do
  assertEq "TestInsertAt" [10, S Z, True, "Yeah"] $ insertAt 0 sample3 10
  assertEq "TestInsertAt" [S Z, 10, True, "Yeah"] $ insertAt 1 sample3 10
  assertEq "TestInsertAt" [S Z, True, 10, "Yeah"] $ insertAt 2 sample3 10

testDeleteAt : IO ()
testDeleteAt = do
  assertEq "DeleteAt" ["Text", 10, 4] $ deleteAt 0 sample4
  assertEq "DeleteAt" [False, 10, 4] $ deleteAt 1 sample4
  assertEq "DeleteAt" [False, "Text", 4] $ deleteAt 2 sample4
  assertEq "DeleteAt" [False, "Text", 10] $ deleteAt 3 sample4

testOrd : IO ()
testOrd = do
  assertEq "TestOrd" True $ sample4 < sample4a
  assertEq "TestOrd" False $ sample4 > sample4a
