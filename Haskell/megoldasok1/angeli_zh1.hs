--Sajnos 2 hiba van benne, egyik kódsorok számozásánál, de attól még lefut(ott) *edit: ez nem hiba
--Másrészt a 38-39. sorban a Link [Inline]-ra a map nem ok *edit: azt hiszem mapM kell
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc
import qualified Data.Text.IO
import Data.Char (toUpper)
import Control.Monad.State

doStuffOn :: Pandoc -> Int -> (Pandoc,[Int])
doStuffOn p c = runState ((walkM checkBlock) p) [c,0,0,0]

countCode :: Block -> [Int]
countCode (CodeBlock a str) = [(length (lines str))]
countCode _ = []

checkBlock :: Block -> State [Int] Block
checkBlock (CodeBlock a str) = do
 i <- get
 let r = runState (mapM numberLine (lines str)) i
 put $ snd r
 pure $ CodeBlock a (foldl (++) "" $ fst r)
checkBlock b = do
 i <- get
 --let r = execState ((walkM checkInline) b) i
 --put r
 --let r = (query checkInline2) b
 --put $ zipWith (+) i [0,0,(length r),(sum r)]
 pure $ evalState ((walkM checkInline) b) i

checkInline :: Inline -> State [Int] Inline
checkInline (Str str) = do
 pure $ (Str "x")
checkInline (Code a str) = do
 pure $ (Code a "x")
checkInline (Link a inl t) = do
 i <- get
 pure $ (Link a (map f inl) t)
  where	f inli = (evalState ((walkM checkInline) inli) 0)
checkInline i = do
 --i <- get
 pure $ i

numberLine :: String -> State [Int] String
numberLine s = do
 i <- get
 put $ zipWith (+) i [(-1),0,0,0] 
 pure $ "# " ++ (show (i!!0)) ++ "\n"

main :: IO ()
main = do    
    s <- Data.Text.IO.readFile "example.md"
    p <- runIOorExplode $ readMarkdown (def {readerExtensions = pandocExtensions}) s;
    let c = sum ((query countCode) p)
    let r = doStuffOn p c
    s' <- runIOorExplode $ writeMarkdown (def {writerExtensions = pandocExtensions}) $ fst r
    Data.Text.IO.writeFile "example2.md" s';
    putStrLn ((show $ c) ++ ":" ++ (show $ (snd r)!!0) ++ " code lines");