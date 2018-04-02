import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO
import Data.Char (toUpper)
import Data.List.Split
import Control.Monad.State.Lazy


newId :: State Int Int
newId = state $ \s -> (s+1, s+1)

numberLine :: String -> State Int String
numberLine s = do
    i <- newId
    pure $ show i ++ ". " ++ s

callNumberLine :: [String] -> [String]
callNumberLine xs = fst (runState (mapM numberLine xs) 0)

--callMultiple xs = [show (callNumberLine (xs !! i)) | i <- [0,1..((length xs)-1)]]

--

--newId :: State Int Int
--newId = state $ \s -> (s+1, s+1)
--
--numberLine :: String -> State Int String
--numberLine s = do
--    i <- newId
--    pure $ show i ++ ". " ++ s
--
--callNumberLine :: [String] -> [String]
--callNumberLine xs = fst (runState (mapM numberLine xs) 0)
--
--splitModJoin :: ([String] -> [String]) -> String -> String
--splitModJoin f s = join (map lnend (f (splitOn "\n" s)))
--    where lnend x = x ++ "\n"
--
--modSources1 :: Block -> Block
--modSources1 (CodeBlock attr xs) = CodeBlock attr (splitModJoin callNumberLine xs)
--modSources1 x = x
--
--enumSources :: Pandoc -> Pandoc
--enumSources = walk modSources1
--
--main = do
----    putStrLn $ splitModJoin callNumberLine "cskdjcnsdjcnsjd\naiusbbnoansiva\nausnaus"
----    putStrLn $ splitModJoin callNumberLine "cskdjcnsdjcnsjd\naiusbbnoansiva\nausnaus"
--    putStrLn $ splitModJoin callNumberLine "cskdjcnsdjcnsjd\naiusbbnoansiva\nausnaus"

--
--type GameValue = Int
--type GameState = (Bool, Int)
--
--playGame :: String -> State GameState GameValue
--playGame []     = do
--    (_, score) <- get
--    return score
--
--playGame (x:xs) = do
--    (on, score) <- get
--    case x of
--         'a' | on -> put (on, score + 1)
--         'b' | on -> put (on, score - 1)
--         'c'      -> put (not on, score)
--         _        -> put (on, score)
--    playGame xs
--
--startState = (False, 0)
--
--main = print $ evalState (playGame "abcaaacbbcabbab") startState



f :: [String] -> ([String], Int)
f x = (x, 2)