
module Test.HList

import Data.Vect
import Data.HList

%access public export
%default total

assertEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertEq g e = if g == e
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

assertNotEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertNotEq g e = if not (g == e)
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

assertHTypes : ( tps : Vect n Type ) ->  HList n tps -> IO ()
assertHTypes tps xs =  putStrLn "Test Passed"

sample3 : HList 3 [Nat, Bool, String]
sample3 = [S Z, True, "Yeah"]

sample3a : HList 3 [Bool, String, Integer]
sample3a = [True, "Yeah", 10]

sample4 : HList 4 [Bool, String, Integer, Nat]
sample4 = [False, "Text", 10, 4]

testLength :  IO ()
testLength = assertEq (length sample3) 3

testGet :  IO ()
testGet =
  do
    assertEq (S Z) (get 0 sample3)
    assertEq True (get 1 sample3)
    assertEq "Yeah" (get 2 sample3)

testSet : IO ()
testSet = do
  assertEq Z $ get 0 $ (set 0 sample3 Z)
  assertEq True $ get 1 $ (set 0 sample3 Z)
  assertEq "Yeah" $ get 2 $ (set 0 sample3 Z)

testHead : IO ()
testHead = assertEq (S Z) $ head sample3

testLast : IO ()
testLast = assertEq "Yeah" $ last sample3

testZip : IO ()
testZip =
  let zs = zip sample3 sample3a
  in do
    assertEq 3 $ length zs
    assertEq (True, "Yeah") $ get 1 zs
    assertEq ("Yeah", 10) $ get 2 zs

testTake : IO ()
testTake =
  let rs = take 2 sample3
  in do
    assertHTypes [Nat, Bool] rs
    assertEq 2 $ length rs
    assertEq True $ get 1 rs

testDrop : IO ()
testDrop =
  let rs = drop 1 sample3
  in do
    assertHTypes [Bool, String] rs
    assertEq True $ get 0 rs

testInit : IO ()
testInit =
  let rs = init sample3
  in do
    assertHTypes [Nat, Bool] rs
    assertEq 2 $ length rs
    assertEq True $ get 1 rs

testTail : IO ()
testTail =
  let rs = tail sample3
  in do
    assertHTypes [Bool, String] rs
    assertEq 2 $ length rs
    assertEq True $ get 0 rs

testConcat : IO ()
testConcat =
  let rs = sample3 ++ sample4
  in do
    assertHTypes [Nat, Bool, String, Bool, String, Integer, Nat] rs
    assertEq 7 $ length rs
    assertEq 10 $ get 6 rs

testUpdate : IO ()
testUpdate =
  let rs = update 3 sample4 "YS"
  in do
    assertHTypes [Bool, String, Integer, String] rs
    assertEq 4 $ length rs
    assertEq "YS" $ get 3 rs

testInsertAt : IO ()
testInsertAt =
  let rs = insertAt 2 sample3 10
  in do
    assertHTypes [Nat, Bool, Integer, String] rs
    assertEq 4 $ length rs
    assertEq 10 $ get 2 rs

testInsertAt0 : IO ()
testInsertAt0 =
  let rs = insertAt 0 sample3 10
  in do
    assertHTypes [Integer, Nat, Bool, String] rs
    assertEq 4 $ length rs
    assertEq 10 $ get 0 rs

testInsertAt3 : IO ()
testInsertAt3 =
  let rs = insertAt 3 sample3 10
  in do
    assertHTypes [Nat, Bool, String,Integer] rs
    assertEq 4 $ length rs
    assertEq 10 $ get 3 rs

testDeleteAt : IO ()
testDeleteAt =
  let rs = deleteAt 2 sample4
  in do
    assertHTypes [Bool, String, Nat] rs
    assertEq 3 $ length rs
