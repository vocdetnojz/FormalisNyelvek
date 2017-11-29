module Turing

import StdEnv, StdLib, StdGeneric, GenEq

// Zipper adattipus
:: Zipper a = {
	left :: [a],
	right :: [a]
	}

Z :: [a] [a] -> Zipper a
Z ls rs = { left = ls, right = rs }

derive gEq Zipper

// Zipper letrehozasa listabol
fromList :: [a] -> Zipper a
fromList a = { left = [], right = a}

// Kijelolt elem lekerdezese
read :: (Zipper a) -> a
read a = hd a.right

// Kijelolt elem modositasa
write :: a (Zipper a) -> Zipper a
write a b = Z b.left (updateAt 0 a b.right)

// Zipper leptetese
:: Movement = Forward | Backward | Stay

move :: Movement (Zipper a) -> Zipper a
move Forward a = Z ([hd a.right] ++ a.left) (tl a.right)
move Backward a = Z (tl a.left) ([hd a.left] ++ a.right)
move Stay a = Z a.left a.right

// Kijelolt elem kornyeke
around :: Int (Zipper a) -> [a]
around a b = (reverse (take a b.left)) ++ [hd b.right] ++ (take a (tl b.right))

// Vegtelen zipper letrehozasa listabol
fromListInf :: a [a] -> Zipper a
fromListInf a b = Z (repeat a) (b ++ (repeat a))

// Turing-gep tipusosztaly
class Machine t where
  done :: (t a) -> Bool
  tape :: (t a) -> Zipper a
  step :: (t a) -> t a

// Turing-gep adattipus
:: State = InState Int | Accepted | Rejected

derive gEq State

:: TuringMachine a = {
	state :: State,
	zipper :: Zipper a,
	f :: Int a -> (State, a, Movement)
	}

TM :: State (Zipper a) (Int a -> (State, a, Movement)) -> TuringMachine a
TM st zp fu = { state=st, zipper=zp, f=fu }

// A Turing-gep mukodese

instance Machine TuringMachine where
  done a
  	| a.state === Accepted = True
  	| a.state === Rejected = True
  	| otherwise = False
  tape a = a.zipper
  step a = TM n_state (move n_move (write n_symbol a.zipper)) a.f
  	where
  		( n_state, n_symbol, n_move ) = f state (read zipper)
  			where
  				f = a.f
  				state = a.state
  				zipper = a.zipper
  		
// FIXME a.func itt csak referencia, nem hivas - f(a.stat, (read a.zipp))


////////// EDDIG JO //////////
/*
// A Turing-gep futtatasa
run :: (t a) -> [t a] | Machine t
run a = abort "not defined"

// Turing-gep allapotainak megjelenitese
showStates :: (t Char) -> [String] | Machine t
showStates a = abort "not defined"
*/
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

/*
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
    f 1 _   = (Accepted,  'x', Stay)*/
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
//  , test_done
//  , test_tape
//  , test_step
//  , test_run
//  , test_showStates
  ]


//Start = (all and tests, zip2 [1..] (map and tests))
Start = InState 0