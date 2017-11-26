module Turing

import StdEnv, StdLib, StdGeneric, GenEq

:: Zipper a = Something

derive gEq Zipper

fromList :: [a] -> Zipper a
fromList a = abort "not defined"

read :: (Zipper a) -> a
read a = abort "not defined"

write :: a (Zipper a) -> Zipper a
write a b = abort "not defined"

:: Movement = Something1

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
Start = ""