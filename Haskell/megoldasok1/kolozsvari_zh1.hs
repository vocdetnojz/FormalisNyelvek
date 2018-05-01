
import Text.Pandoc
import Text.Pandoc.Walk
import Data.Text (pack, unpack)
import Data.List.Split
import Data.Monoid
import Control.Monad.State.Lazy

-- Fájlból beolvasás esetén a '\r' karakterek megzavarhatják az AST építését
eliminateR :: String -> String
eliminateR (x:xs) | x == '\r' = eliminateR xs
                  | otherwise = [x] ++ (eliminateR xs)
eliminateR [] = []

--------------------------------------------------------------
-- 2-es

-- Kódblokkon belüli sorok számozása
numberLines :: ([String], Int) -> ([String], Int)
numberLines ((x:xs), n) = ((["#" ++ (show n) ++ "\n"] ++ (fst (numberLines(xs, n-1)))),n)
numberLines (x, 0) = (x, 0)
numberLines ([], n) = ([], n)

codeNum :: Block -> State Int Block
codeNum (CodeBlock a s) = do
                           p <- get
                           let lines = (splitOn "\n" s)
                           put (p - (length lines))
                           return $ CodeBlock a $ concat $ fst $ numberLines (lines, p)
codeNum x = return x

countCodeBlocks :: Pandoc -> Sum Int
countCodeBlocks = query cblock
        where cblock (CodeBlock a s) = Sum $ length (splitOn "\n" s)
              cblock _ = mempty

--------------------------------------------------------------
-- 3-as

replaceWithX :: Inline -> Inline
replaceWithX (Str s) = Str $ changeCharsX s
replaceWithX (Code a s) = Code a $ changeCharsX s
replaceWithX (Link a i (x,y)) = Link  a i (changeCharsX x, changeCharsX y)
replaceWithX x = x

changeCharsX :: String -> String
changeCharsX xs = map (\i -> 'x') xs

--------------------------------------------------------------
-- 4-es

mapChar :: Int{-összes karakter-} -> Int{-karakter sorszáma-} -> Char
mapChar all i = toEnum $ fromEnum 'a' + (26 * i `div` all)

countChar :: Pandoc -> Sum Int
countChar = query characters
        where characters (Str xs) = Sum $ length xs
              characters _ = mempty

replaceWith :: Inline -> Inline
replaceWith (Str s) = Str $ changeCharsX s
replaceWith x = x

--------------------------------------------------------------

main :: IO ()
main = do
  s <- readFile("example.md")
  ast <- runIOorExplode $ readMarkdown def {readerExtensions = pandocExtensions} $ pack $ eliminateR s
  let n = getSum $ countCodeBlocks ast
  let codeBlockRes = runState (walkM codeNum ast) n
  let c = getSum $ countChar ast
  result <- runIOorExplode $ writeMarkdown def {writerExtensions = pandocExtensions} $ walk replaceWithX $ fst codeBlockRes
  writeFile "3as.md" $ unpack result 
  -- result <- runIOorExplode $ writeMarkdown def {writerExtensions = pandocExtensions} $ walk replaceWithX $ fst codeBlockRes
  -- writeFile "4es.md" $ unpack result 
