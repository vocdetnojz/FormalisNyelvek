module Mastermind

import StdEnv

positionalMatches :: [Int] [Int] -> Int
positionalMatches xs ys = length [(x,y) \\ (x,y) <- zip(xs, ys) | x == y]

matches :: [Int] [Int] -> Int
matches a b = length b - length (removeMembers a b)

:: Nothing = Nil
:: Just :== [Int]
:: Maybe = Nothing | Just [Int]

instance == Maybe where
	(==) Nothing Nothing = True
	(==) (Just a) (Just b) = a == b
	(==) _ _ = False

//readCode :: String -> Maybe [Int]
readCode str = if ([1,1,1,1] == [1 \\ x <-: str] && [1,1,1,1] == [1 \\ x <-: str | isDigit x]) (Just [digitToInt x \\ x <-: str]) Nothing

//*********

//maybe :: (a -> b) b (Maybe a) -> b
//maybe f b x = b

//allMatches :: [Int] String -> (Int, Int)
//allMatches a b = (0,0)



//***********************************************//

//positionalMatches_test = [ positionalMatches [4,2,7,1] [1,2,3,4] == 1, positionalMatches [9,3,0,5] [5,6,7,8] == 0, positionalMatches [6,6,6,1] [6,6,5,1] == 3]
//matches_test =[ matches [4,2,7,1] [1,2,3,4] == 3, matches [9,3,0,5] [5,6,7,8] == 1, matches [6,6,6,1] [6,6,5,1] == 3, matches [5,8,7,9] [9,9,7,8] == 3]
readCode_test =[ readCode "1234"  == Just [1,2,3,4], readCode "12345" == Nothing, readCode "123a"  == Nothing, readCode "1234a" == Nothing]
//maybe_test =[ maybe ((+) 10) 7 Nothing  == 7, maybe ((+) 10) 7 (Just 5) == 15]
//allMatches_test =[ allMatches [4,2,7,1] "1234" == (2, 1), allMatches [9,3,0,5] "1234" == (1, 0), allMatches [9,3,0,5] "123a" == (0, 0)]
  
Start = readCode_test