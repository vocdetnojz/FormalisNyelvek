import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO
import Data.Char (toUpper)
import Data.List.Split
import Control.Monad.State.Lazy
import Control.Monad


-- A ---------------------------------------------

newId :: State Int Int
newId = state $ \s -> (s+1, s+1)

numberLine :: String -> State Int String
numberLine s = do
    i <- newId
    pure $ show i ++ "   " ++ s

callNumberLine :: [String] -> [String]
callNumberLine xs = fst (runState (mapM numberLine xs) 0)

splitModJoin :: ([String] -> [String]) -> String -> String
splitModJoin f s = join (map lnend (f (splitOn "\n" s)))
    where lnend x = x ++ "\n"

modSources1 :: Block -> Block
modSources1 (CodeBlock attr xs) = CodeBlock attr (splitModJoin callNumberLine xs)
modSources1 x = x

enumSources :: Pandoc -> Pandoc
enumSources = walk modSources1

-- B ---------------------------------------------

---- FIXME ez a resz hibat dob, nem tudom lehozni a megfelelo helyre a newId-t, mert ha nem kozos peldanyt hasznalok a newId fuggvenybol, akkor ujraindul az Id kiosztas
--
--newId :: State Int Int
--newId = state $ \s -> (s, s+1)
--
--getId :: State Int StateT -> State Int String -- FIXME: Expecting three more arguments to `StateT': Expected a type, but `StateT' has kind `* -> (* -> *) -> * -> *'
--getId gen = do
--    i <- gen
--    pure $ show i
--
--numberLine :: String -> State Int Int -> String
--numberLine s gen = (f gen) ++ "   " ++ s ++ "\n"
--    where f = fst $ runState (mapM getId gen)
--
--callNumberLine :: [String] -> State Int Int -> [String]
--callNumberLine xs gen = [(numberLine s gen) | s <- xs]
--
--splitModJoin :: String -> State Int Int -> String
--splitModJoin xs gen = join (callNumberLine (splitOn "\n" xs) gen)
--
--evalOnBlock :: Block -> State Int Block
--evalOnBlock (CodeBlock attr xs) = do
----    i <- newId
--    pure $ CodeBlock attr (splitModJoin xs newId)
--evalOnBlock x = pure x
--
--enumSources :: Pandoc -> Pandoc
--enumSources p = evalState (walkM evalOnBlock p) 1

-- C ---------------------------------------------

newIdC :: State Int Int
newIdC = state $ \s -> (s, s+1)

numberLineC :: String -> State Int String
numberLineC s = do
    i <- newIdC
    pure $ s ++ "\n"

callNumberLineC :: [String] -> Int -> ([String], Int)
callNumberLineC xs a = runState (mapM numberLineC xs) a

splitModJoinC :: String -> Int -> (String, Int)
splitModJoinC xs a = (join (fst (callNumberLineC (splitOn "\n" xs) a)), (snd (callNumberLineC (splitOn "\n" xs) a)))

evalOnBlockC :: Block -> State Int Block
evalOnBlockC (CodeBlock attr xs) = do
    i <- newIdC
    pure $ CodeBlock attr (fst (splitModJoinC xs i))
evalOnBlockC x = pure x

statCSources :: Pandoc -> Int
statCSources p = execState (walkM evalOnBlockC p) 0

-- D ---------------------------------------------


newIdD :: State Int Int
newIdD = state $ \s -> (s, s+1)

numberLineD :: String -> State Int String
numberLineD s = do
    i <- newIdD
    pure $ s ++ "\n"

callNumberLineD :: [String] -> Int -> ([String], Int)
callNumberLineD xs a = runState (mapM numberLineD xs) a

splitModJoinD :: String -> Int -> (String, Int)
splitModJoinD xs a = (join (fst (callNumberLineD (splitOn "\n" xs) a)), (snd (callNumberLineD (splitOn "\n" xs) a)))

evalOnBlockD :: Block -> State Int Block
evalOnBlockD (CodeBlock attr xs) = do
    i <- newIdD
    pure $ CodeBlock attr (fst (splitModJoinD xs i))
evalOnBlockD x = pure x

statDSources :: Pandoc -> Int
statDSources p = execState (walkM evalOnBlockD p) 0

----------------------------------------------------

main :: IO ()
main = do
--    putStrLn "file name: "
--    name <- getLine
--    s <- Data.Text.IO.readFile name
    s <- Data.Text.IO.readFile "example.md"
    p <- runIOorExplode $ readMarkdown def s
    s <- runIOorExplode $ writeMarkdown (def { writerSetextHeaders = False }) $ enumSources p
    putStrLn $ show (statDSources p) ++ " code blocks"
    putStrLn $ show (statCSources p) ++ " code lines"
--    codeLineCount <- newId
--    putStrLn $ show
--    Data.Text.IO.writeFile (name ++ "_converted.md") s
    Data.Text.IO.writeFile "example2.md" s
