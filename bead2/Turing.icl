module Turing

import StdEnv, StdLib, StdGeneric, GenEq

:: Zipper a = {
	left :: [a],
	right :: [a]
	}

Z :: [a] [a] -> Zipper a
Z ls rs = { left = ls, right = rs }

derive gEq Zipper

fromList :: [a] -> Zipper a
fromList a = { left = [], right = a}

read :: (Zipper a) -> a
read a = hd a.right

write :: a (Zipper a) -> Zipper a
write a b = Z b.left (updateAt 0 a b.right)

// todo
:: Movement = Zipper

Forward :: (Zipper a) -> (Zipper a)
Forward a = Z ([hd a.right] ++ a.left) (tl a.right)

Backward :: (Zipper a) -> (Zipper a)
Backward a = Z (tl a.left) ([hd a.left] ++ a.right)

Stay :: (Zipper a) -> (Zipper a)
Stay a = Z a.left a.right


move :: Movement (Zipper a) -> Zipper a
move a b = abort "not defined"

around :: Int (Zipper a) -> [a]
around a b = abort "not defined"

fromListInf :: a [a] -> Zipper a
fromListInf a b = abort "not defined"

class Machine t where
  done :: (t a) -> Bool
  tape :: (t a) -> Zipper a
  step :: (t a) -> t a

:: State = Something2

:: TuringMachine a = Something3

instance Machine TuringMachine where
  done a = abort "undefined"
  tape a = abort "undefined"
  step a = abort "undefined"

run :: (t a) -> [t a] | Machine t
run a = abort "not defined"

showStates :: (t Char) -> [String] | Machine t
showStates a = abort "not defined"

// TESTS

test_fromList = [ fromList empty === Z [] [], fromList [1] === Z [] [1], fromList [1..10] === Z [] [1..10], fromList [1..10] =!= Z [0] [1..10]]
  where
    empty :: [Int]
    empty = []
    
test_read = [ read (Z [] [1]) == 1, read (Z [] [2..]) == 2, read (Z [1..] [3..]) == 3]
  
test_write = [ write 9 (Z [] [1]) === Z [] [9], write 9 (Z [] [1..3]) === Z [] [9,2,3], write 9 (Z [4..6] [1..3]) === Z [4..6] [9,2,3]]
/*
test_move =
  [ move Stay (Z empty [])            === Z [] []
  , move Stay (Z [1,2,3] [4,5,6])     === Z [1,2,3] [4,5,6]
  , move Forward (Z [1,2,3] [4,5,6])  === Z [4,1,2,3] [5,6]
  , move Backward (Z [1,2,3] [4,5,6]) === Z [2,3] [1,4,5,6]
  ]
  where
    empty :: [Int]
    empty = []
*/
/*
tests :: [[Bool]]
tests =
  [ test_fromList
  , test_read
  , test_write
  , test_move
  , test_around
  , test_fromListInf
  , test_done
  , test_tape
  , test_step
  , test_run
  , test_showStates
  ]
*/

//Start = (all and tests, zip2 [1..] (map and tests))
Start = Stay (Z [] [])