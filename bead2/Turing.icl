module Turing

import StdEnv, StdLib, StdGeneric, GenEq

// 1 	Zipper adattipus
// 1 	Zipper letrehozasa listabol
// 1 	Kijelolt elem lekerdezese
// 1 	Kijelolt elem modositasa
// 1 	Zipper leptetese
// 1 	Kijelolt elem kornyeke
// ? 	Vegtelen Zipper letrehozasa listabol
// 1 	Turing-gep tipusosztaly
// 1 	Turing-gep adattipus
// 0	A Turing-gep mukodese
// 0	A Turing-gep futtatasa
// 0	Turing-gep allapotainak megjelenitese

// Zipper adatt�pus
:: Zipper a = {
	left :: [a],
	right :: [a]
	}

Z :: [a] [a] -> Zipper a
Z ls rs = { left = ls, right = rs }

derive gEq Zipper

// Zipper l�trehoz�sa list�b�l
fromList :: [a] -> Zipper a
fromList a = { left = [], right = a}

// Kijel�lt elem lek�rdez�se
read :: (Zipper a) -> a
read a = hd a.right

// Kijel�lt elem m�dos�t�sa
write :: a (Zipper a) -> Zipper a
write a b = Z b.left (updateAt 0 a b.right)

// Zipper l�ptet�se
:: Movement = Forward | Backward | Stay

move :: Movement (Zipper a) -> Zipper a
move Forward a = Z ([hd a.right] ++ a.left) (tl a.right)
move Backward a = Z (tl a.left) ([hd a.left] ++ a.right)
move Stay a = Z a.left a.right

// Kijel�lt elem k�rny�ke
around :: Int (Zipper a) -> [a]
around a b = (reverse (take a b.left)) ++ [hd b.right] ++ (take a (tl b.right))

// V�gtelen zipper l�trehoz�sa list�b�l
fromListInf :: a [a] -> Zipper a
fromListInf a b = Z (repeat a) (b ++ (repeat a))

// Turing-g�p t�pusoszt�ly
class Machine t where
  done :: (t a) -> Bool
  tape :: (t a) -> Zipper a
  step :: (t a) -> t a

// Turing-g�p adatt�pus
:: State = InState Int | Accepted | Rejected

derive gEq State

:: TuringMachine a = {
	stat :: State,
	zipp :: Zipper a,
	func :: Int a -> (State, a, Movement)
	}

TM :: State (Zipper a) (Int a -> (State, a, Movement)) -> TuringMachine a
TM st zp fu = { stat=st, zipp=zp, func=fu }

// A Turing-g�p m�k�d�se
instance Machine TuringMachine where
  done a
  	| a.stat === Accepted = True
  	| a.stat === Rejected = True
  	| otherwise = False
  tape a = a.zipp
  step a = move a.zipp a  // FIXME


////////// EDDIG J� //////////

// A Turing-g�p futtat�sa
run :: (t a) -> [t a] | Machine t
run a = abort "not defined"

// Turing-g�p �llapotainak megjelen�t�se
showStates :: (t Char) -> [String] | Machine t
showStates a = abort "not defined"

///////////////////////////// TESTS ///////////////////////////////////

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


test_done =
  [ not (done (TM (InState 0) undef undef))
  , done (TM Accepted undef undef)
  , done (TM Rejected undef undef)
  ]

test_tape =
  [ tape (TM Accepted (fromList [1..5]) undef) === fromList [1..5]
  ]

test_step =
  [ let m = step (TM (InState 0) (fromList ['a','b']) f)
    in  not (done m)
        && tape m === Z ['b'] ['b']
  , let m = step (TM (InState 0) (fromList ['b','b']) f)
    in  not (done m)
        && tape m === Z ['a'] ['b']
  , let m = step (TM (InState 1) (fromList ['a','b']) f)
    in  done m
        && tape m === fromList ['x','b']
  ]
  where
    f 0 'a' = (InState 0, 'b', Forward)
    f 0 'b' = (InState 0, 'a', Forward)
    f 1 _   = (Accepted,  'x', Stay)
/*
test_run =
  [ let m = last (run (tm ['a','b','x','x']))
    in done m
       && tape m === Z ['x','a','b'] ['x']
  , let m = last (run (tm ['b','a','x','x']))
    in done m
       && tape m === Z ['x','b','a'] ['x']
  , let m = last (run (tm ['a','b','x','a']))
    in done m
       && tape m === Z ['x','a','b'] ['!']
  ]
  where
    tm xs = TM (InState 0) (fromList xs) f
    f 0 'a' = (InState 0, 'b', Forward)
    f 0 'b' = (InState 0, 'a', Forward)
    f 0 'x' = (InState 1, 'x', Forward)
    f 1 'x' = (Accepted,  'x', Stay)
    f _ ch  = (Rejected,  '!', Stay)
    
test_showStates =
  [ showStates (tm ['a','b','x','x'])
    == [ "     abxx  "
       , "    bbxx   "
       , "   baxx    "
       , "  baxx     "
       , "  baxx     "
       ]
  , showStates (tm ['a','b','x','a'])
    == [ "     abxa  "
       , "    bbxa   "
       , "   baxa    "
       , "  baxa     "
       , "  bax!     "
       ]
  ]
    where
      tm xs = TM (InState 0) (fromListInf ' ' xs) f
      f 0 'a' = (InState 0, 'b', Forward)
      f 0 'b' = (InState 0, 'a', Forward)
      f 0 'x' = (InState 1, 'x', Forward)
      f 1 'x' = (Accepted,  'x', Stay)
      f _ ch  = (Rejected,  '!', Stay)
*/

tests :: [[Bool]]
tests =
  [ test_fromList
  , test_read
  , test_write
  , test_move
  , test_around
//  , test_fromListInf
  , test_done
  , test_tape
  , test_step
//  , test_run
//  , test_showStates
  ]


//Start = (all and tests, zip2 [1..] (map and tests))
Start = test_step