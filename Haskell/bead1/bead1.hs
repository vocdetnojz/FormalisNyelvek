import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO
import Data.Char (toUpper)
import Data.List.Split


-- A ---------------------------------------------

enumerate :: String -> String
enumerate x = concat [(show (i+1)) ++ "   " ++ (strings !! i) ++ "\n" | i <- [0,1..((length strings)-1)]]
    where strings = splitOn "\n" x

modSources1 :: Block -> Block
modSources1 (CodeBlock attr xs) = CodeBlock attr (enumerate xs)
modSources1 x = x

enumSources :: Pandoc -> Pandoc
enumSources = walk modSources1

-- B ---------------------------------------------



--------------------------------------------------

main :: IO ()
main = do
--    putStrLn "file name: "
--    name <- getLine
--    s <- Data.Text.IO.readFile name
    s <- Data.Text.IO.readFile "example.md"
    p <- runIOorExplode $ readMarkdown def s
    s <- runIOorExplode $ writeMarkdown (def { writerSetextHeaders = False }) $ enumSources p
--    Data.Text.IO.writeFile (name ++ "_converted.md") s
    Data.Text.IO.writeFile "example2.md" s

