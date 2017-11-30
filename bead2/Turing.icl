module Turing

import StdEnv, StdLib, StdGeneric, GenEq

:: Zipper a = Z [a] [a]

derive gEq Zipper

fromList :: [a] -> Zipper a
fromList a = Z [] a

read :: (Zipper a) -> a
read (Z x [y:ys]) = y

write :: a (Zipper a) -> Zipper a
write a (Z x [y:ys]) = Z x [a:ys]

:: Movement = Forward | Backward | Stay

move :: Movement (Zipper a) -> Zipper a
move Forward (Z x [y:ys]) = Z [y:x] ys
move Backward (Z [x:xs] y) = Z xs [x:y]
move Stay b = b

around :: Int (Zipper a) -> [a]
around a (Z x [y:ys]) = reverse (taker a x) ++ [y] ++ (taker a ys)
	where
		taker a [] = []
		taker 0 [x:xs] = []
		taker a [x:xs] = [x:(taker (a - 1) xs)]
		
fromListInf :: a [a] -> Zipper a
fromListInf r x = Z (repeat r) (x ++ (repeat r))

class Machine t where
  done :: (t a) -> Bool
  tape :: (t a) -> Zipper a
  step :: (t a) -> t a

:: State = InState Int | Accepted | Rejected

:: TuringMachine a = TM State (Zipper a) (Int a -> (State, a, Movement))

instance Machine TuringMachine where
  done (TM (InState _) _ _) = False
  done (TM Accepted _ _) = True
  done (TM Rejected _ _) = True
  tape (TM _ b _) = b
  step (TM (InState num) zipper fv) = (TM state (move mov (write val zipper)) fv)
  	where
  		(state, val, mov) = fv num (read zipper)
  	
run :: (t a) -> [t a] | Machine t
run a
  | done a == True = [a]
  | otherwise = [a] ++ run (step a) 

showStates :: (t Char) -> [String] | Machine t
showStates a
  | done a == True = [toString (around 5 (tape a))]
  |	otherwise = [toString (around 5 (tape a))] ++ showStates (step a)

/////////////////

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

test_fromListInf =
  [ let (Z xs ys) = fromListInf 0 [1..5]
    in  take 100 xs == repeatn 100 0
        && take 100 ys == [1..5] ++ repeatn 95 0
  ]

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

Start = (all and tests, zip2 [1..] (map and tests))