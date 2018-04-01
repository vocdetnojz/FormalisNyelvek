import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO
import Data.Char (toUpper)
import Data.List.Split

splitLines :: String -> [String]
splitLines [] = []
splitLines all@(x:xs) = splitOn "\n" all

enumerator :: [String] -> [String]
enumerator [] = []
enumerator [x:xs] = []


main = do
    putStr result
    where
        strings = ["a", "fucking", "string", "list"]
        result = concat [(show (i+1)) ++ "   " ++ (strings !! i) ++ "\n" | i <- [0,1..((length strings)-1)]]



