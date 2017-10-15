module Mastermind

import StdEnv 
import StdLib

// 1
positionalMatches :: [Int] [Int] -> Int
positionalMatches xs ys = length [(x,y) \\ (x,y) <- zip(xs, ys) | x == y]

// 2

matches :: [Int] [Int] -> Int
matches a b 
  | length b - length (removeMembers a b) < 0 = 0
  | otherwise = length b - length (removeMembers a b)

// 3
toChars :: String -> [Char]
toChars str = fromString str

chkLen :: [Char] -> [Char]
chkLen str
  | length str == 4 = str
  | otherwise = []

toInts :: [Char] -> [Int]
toInts [] = []
toInts [x:xs] 
  | (isDigit x == True) = [digitToInt x] ++ (toInts xs)
  | otherwise = toInts xs

validate :: [Int] -> Maybe [Int]
validate [] = Nothing
validate x
  | (length x == 4) = Just x
  | otherwise = Nothing
  
readCode :: String -> Maybe [Int]
readCode str = validate (toInts (chkLen (toChars str)))

// 4
maybe :: (a -> b) b (Maybe a) -> b
maybe f b Nothing = b
maybe f b (Just a) = f a

// 5

allMatches :: [Int] String -> (Int, Int)
allMatches a b = (matches a (maybe ((++) []) [] (readCode b)), positionalMatches a (maybe ((++) []) [] (readCode b)))


positionalMatches_test =
  [ positionalMatches [4,2,7,1] [1,2,3,4] == 1
  , positionalMatches [9,3,0,5] [5,6,7,8] == 0
  , positionalMatches [6,6,6,1] [6,6,5,1] == 3
  ]
matches_test =
  [ matches [4,2,7,1] [1,2,3,4] == 3
  , matches [9,3,0,5] [5,6,7,8] == 1
  , matches [6,6,6,1] [6,6,5,1] == 3
  , matches [5,8,7,9] [9,9,7,8] == 3
  ]
readCode_test =
  [ readCode "1234"  == Just [1,2,3,4]
  , readCode "12345" == Nothing
  , readCode "123a"  == Nothing
  , readCode "1234a"  == Nothing
  ]
maybe_test =
  [ maybe ((+) 10) 7 Nothing  == 7
  , maybe ((+) 10) 7 (Just 5) == 15
  ]
allMatches_test =
  [ allMatches [4,2,7,1] "1234" == (3, 1)
  , allMatches [9,3,0,5] "1234" == (1, 0)
  , allMatches [9,3,0,5] "123a" == (0, 0)
  ]


Start = allMatches_test