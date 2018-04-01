import Data.List
import System.IO

-- Int -2^63 2^63
-- Integer
-- Double up to 11 points
-- Bool True False
-- Char '
-- Tuple
-- always5 :: Int 
-- always5 = 5

sumOfNums = sum [1..1000]
addEx = 5 + 4
subEx = 5 - 4
mulEx = 5 * 4
divEx = 5 / 4

modEx = mod 5 4 -- prefix operator
modEx2 = 5 `mod` 4 -- put prefix operator as an infix 

negNumEx = 5 + (-4)

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9) -- sqrt takes float as parameter, convert the int

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

--------

primeNumbers = [3,5,7,11]
morePrime = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : 66 :[]

mulList = [[3, 5, 7], [11, 13, 17]]

lenPrimeNumbers = length primeNumbers

revPrimeNumbers = reverse primeNumbers

isListEmpty = null primeNumbers

secondPrime = primeNumbers !! 1
thirdPrime = primeNumbers !! 2

firstPrime = head primeNumbers
lastPrime = last primeNumbers

primeInit = init primeNumbers

first2Primes = take 2 primeNumbers
removePrimes = drop 2 primeNumbers

is7InList = 7 `elem` primeNumbers
maxPrime = maximum primeNumbers
minPrime = minimum primeNumbers
newList = [2,3,5]
sumPrimes = sum newList
prodPrimes = product newList

zeroToTen = [0..10]
evenList = [2,4..20]
letterList = ['A','C'..'Z']

infinPow10 = [10,20..]
many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10 (cycle [1,2,3,4,5])
listTimes2 = [x * 2 | x <- [1..10]]
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]
divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [2,5,1,1,6,7]
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]
listBiggerThen5 = filter (>5) morePrime
evensUpTo20 = takeWhile (<= 20) [2,4..]

multOfListL = foldl (*) 1 [2,3,4,5]
multOfListR = foldr (*) 1 [2,3,4,5]

multOfListTest = foldr (*) 2 [2,3,4,5]

pow3List = [3^n | n<- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]


---------


randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith",52)
bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]

namesNAddresses = zip names addresses




