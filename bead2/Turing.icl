module Turing

import StdEnv, StdLib, StdGeneric, GenEq

// 1 	Zipper adattipus
// 1 	Zipper letrehozasa listabol
// 1 	Kijelolt elem lekerdezese
// 1 	Kijelolt elem modositasa
// 1 	Zipper leptetese
// 1 	Kijelolt elem kornyeke
// ? 	Vegtelen Zipper letrehozasa listabol
// 0 	Turing-gep tipusosztaly
// 0 	Turing-gep adattipus
// 0	A Turing-gep mukodese
// 0	A Turing-gep futtatasa
// 0	Turing-gep allapotainak megjelenitese

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

:: Movement = Forward | Backward | Stay

move :: Movement (Zipper a) -> Zipper a
move Forward a = Z ([hd a.right] ++ a.left) (tl a.right)
move Backward a = Z (tl a.left) ([hd a.left] ++ a.right)
move Stay a = Z a.left a.right

around :: Int (Zipper a) -> [a]
around a b = (reverse (take a b.left)) ++ [hd b.right] ++ (take a (tl b.right))

// eddig jo

fromListInf :: a [a] -> Zipper a
fromListInf a b = Z (repeat a) (b ++ (repeat a))


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

test_fromList =
  [ fromList empty   === Z [] []
  , fromList [1]     === Z [] [1]
  , fromList [1..10] === Z [] [1..10]
  ]
  where
    empty :: [Int]
    empty = []
    
test_read =
  [ read (Z [] [1])      == 1
  , read (Z [] [2..])    == 2
  , read (Z [1..] [3..]) == 3
  ]
  
test_write =
  [ write 9 (Z [] [1])        === Z [] [9]
  , write 9 (Z [] [1..3])     === Z [] [9,2,3]
  , write 9 (Z [4..6] [1..3]) === Z [4..6] [9,2,3]
  ]
  
test_move =
  [ move Stay (Z empty [])            === Z [] []
  , move Stay (Z [1,2,3] [4,5,6])     === Z [1,2,3] [4,5,6]
  , move Forward (Z [1,2,3] [4,5,6])  === Z [4,1,2,3] [5,6]
  , move Backward (Z [1,2,3] [4,5,6]) === Z [2,3] [1,4,5,6]
  ]
  where
    empty :: [Int]
    empty = []
    
test_around =
  [ around 0 (Z [] [1])      == [1]
  , around 3 (Z [1..] [0..]) == [3,2,1,0,1,2,3]
  ]
  
/*test_fromListInf =
  [ let (Z xs ys) = fromListInf 0 [1..5]
    in  take 100 xs == repeatn 100 0
        && take 100 ys == [1..5] ++ repeatn 95 0
  ]
*/

tests :: [[Bool]]
tests =
  [ test_fromList
  , test_read
  , test_write
  , test_move
  , test_around
//  , test_fromListInf
//  , test_done
//  , test_tape
//  , test_step
//  , test_run
//  , test_showStates
  ]


//Start = (all and tests, zip2 [1..] (map and tests))
Start = ""