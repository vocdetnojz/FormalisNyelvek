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

--newId :: State Int Int
--newId = state $ \s -> (s+1, s+1)
--
--numberLine :: String -> State Int String
--numberLine s = do
--    i <- newId
--    pure $ show i ++ "   " ++ s
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

-- B ---------------------------------------------

newId :: State Int Int
newId = state $ \s -> (s, s+1)

numberLine :: String -> State Int String
numberLine s = do
    i <- newId
    pure $ show i ++ "   " ++ s

callNumberLine :: [String] -> Int -> [String]
callNumberLine xs a = fst $ runState (mapM numberLine xs) a

splitModJoin :: String -> Int -> String
splitModJoin s a = join $ map lnend (callNumberLine (splitOn "\n" s) a)
    where lnend x = x ++ "\n"

evalOnBlock :: Block -> State Int Block
evalOnBlock (CodeBlock attr xs) = do
    i <- newId
    pure $ CodeBlock attr (splitModJoin xs i)
evalOnBlock x = pure x

enumSources :: Pandoc -> Pandoc
enumSources p = evalState (walkM evalOnBlock p) 1

--------------------------------------------------

main :: IO ()
main = do
--    putStrLn "file name: "
--    name <- getLine
--    s <- Data.Text.IO.readFile name
    s <- Data.Text.IO.readFile "example.md"
    p <- runIOorExplode $ readMarkdown def s
    s <- runIOorExplode $ writeMarkdown (def { writerSetextHeaders = False }) $ enumSources p
--    codeLineCount <- newId
--    putStrLn $ show
--    Data.Text.IO.writeFile (name ++ "_converted.md") s
    Data.Text.IO.writeFile "example2.md" s
