import Data.List
import System.IO

main = do
    putStrLn "What's your name mf"
    name <- getLine
    putStrLn ("hello " ++ name)
    
addMe :: Int -> Int -> Int
-- funcname param1 param2 = operations (returned value)
addMe x y = x + y

-- no definition -> will work with any type which has + defined
sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x,y) (x2,y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "you can drive"
whatAge 18 = "you can vote"
whatAge 21 = "you are and adult"
whatAge _ = "youre a bitch"


-- "Recursion" mintaillesztés
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


-- "Guards" esetszétbontás
isOdd :: Int -> Bool
isOdd n 
    |   n `mod` 2 == 0 = False
    |   otherwise = True

    
teamRating :: Double -> Double -> String
teamRating wins loses
    |   avg <= 0.350 = "Sunday League Team"
    |   avg <= 0.450 = "Standard Team"
    |   avg <= 0.570 = "Play-Off Contender Team"
    |   avg <= 0.700 = "Play-Off Team"
    |   otherwise = "Dynasty Team"
    where avg = wins / (wins + loses)

    
getListItems :: [Int] -> String

getListItems [] = "Empty shit"
getListItems (x:[]) = "First " ++ show x
getListItems (x:y:[]) = "Contains: " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "First " ++ show x ++ " and the rest are " ++ show xs

-- "Pattern"
getFirstItem :: String -> String
getFirstItem [] = "Empty shit"
getFirstItem all@(x:xs) = "First in " ++ all ++ " is " ++ show x


-- magasabb rendű fügvények
times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

mulBy4 :: [Int] -> [Int]
mulBy4 [] = []
mulBy4 (x:xs) = times4 x : mulBy4 xs


areStrinsEqual :: [Char] -> [Char] -> Bool
areStrinsEqual [] [] = True
areStrinsEqual (x:xs) (y:ys) = x == y && (areStrinsEqual xs ys)
areStrinsEqual _ _ = False
    
    -- receive a function
doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4

    -- return a function
getAddFunction :: Int -> (Int -> Int)

getAddFunction x y = x + y

increment = getAddFunction 1

incNum = increment 5


-- lambdas
dbl1To10 = map (\x -> x * 2) [1..10]

-- conditionals 


-- monads

