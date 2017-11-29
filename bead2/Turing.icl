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

GetInt :: State -> Int
GetInt a
  | a === Accepted = -9999999
  | a === Rejected = -9999999
  | otherwise = x
  		where	
  			x = hd [x1 \\ x1 <- [-999999..1000000] | a === (InState x1)]

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
  		( n_state, n_symbol, n_move ) = a.f state (read zipper)
  			where
  				state = GetInt a.state  // fixme: ez így nem foolproof
  				zipper = a.zipper

// A Turing-gep futtatasa
run :: (t a) -> [t a] | Machine t
run a
  | done a == True = [a]
  | otherwise = [a] ++ run (step a) 

////////// EDDIG JO //////////

// Turing-gep allapotainak megjelenitese
showStates :: (t Char) -> [String] | Machine t
showStates a = abort "not defined"

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

Start = test_run