module Mastermind

import StdEnv

//******************************
// Egyezések helyenként (2 pont)
//******************************

positionalMatches :: [Int] [Int] -> Int
positionalMatches xs ys = length [(x,y) \\ (x,y) <- zip(xs, ys) | x == y]

// TEST

positionalMatches_test =
  [ positionalMatches [4,2,7,1] [1,2,3,4] == 1
  , positionalMatches [9,3,0,5] [5,6,7,8] == 0
  , positionalMatches [6,6,6,1] [6,6,5,1] == 3
  ]
//Start = positionalMatches_test

//*******************
// Egyezések (3 pont)
//*******************

matches :: [Int] [Int] -> Int
matches a b = length b - length (removeMembers a b)

// TEST

matches_test =
  [ matches [4,2,7,1] [1,2,3,4] == 3
  , matches [9,3,0,5] [5,6,7,8] == 1
  , matches [6,6,6,1] [6,6,5,1] == 3
  , matches [5,8,7,9] [9,9,7,8] == 3
  ]
//Start = matches_test

//************************
// Kód beolvasása (3 pont)
//************************

:: Nothing = Nil
:: Just a = Cons a [Int]
:: Maybe = Nothing | Just

readCode :: String -> Maybe [Int]
readCode str = Nothing

// TEST

readCode_test =
  [ readCode "1234"  == Just [1,2,3,4]
  , readCode "12345" == Nothing
  , readCode "123a"  == Nothing
  ]
Start = readCode_test

//**********************************
// Maybe érték feldolgozása (2 pont)
//**********************************

//maybe :: (a -> b) b (Maybe a) -> b
//maybe f b x = b

// TEST

//maybe_test =
//  [ maybe ((+) 10) 7 Nothing  == 7
//  , maybe ((+) 10) 7 (Just 5) == 15
//  ]

//**************************
// Eredményszámítás (2 pont)
//**************************

//allMatches :: [Int] String -> (Int, Int)
//allMatches a b = (0,0)

// TEST

//allMatches_test =
//  [ allMatches [4,2,7,1] "1234" == (2, 1)
//  , allMatches [9,3,0,5] "1234" == (1, 0)
//  , allMatches [9,3,0,5] "123a" == (0, 0)
//  ]

//*********************************************************
// END 
