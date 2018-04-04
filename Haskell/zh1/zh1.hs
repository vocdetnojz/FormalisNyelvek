import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO
import Data.Char (toUpper)
import Data.List.Split
import Control.Monad.State.Lazy
import Control.Monad

-------------------------

newId :: State Int Int
newId = state $ \s -> (s, s-1)

counter :: State Int Int
counter = state $ \s -> (0, s+1)

walkLines :: Block -> State Int Block
walkLines (CodeBlock attr s) = do
    i <- newId
    pure $ CodeBlock attr ("#" ++ (show i))
walkLines x = do
    pure $ x

doEnumeration :: Pandoc -> Int -> Pandoc
doEnumeration p a = evalState (walkM walkLines p) a

--countLines :: Pandoc -> Int
-- TODO

enumerateBackwards :: Pandoc -> Pandoc
enumerateBackwards p = doEnumeration p 762--(countLines p)



------------------------------------------------------

main :: IO ()
main = do
    s <- Data.Text.IO.readFile "example.md"
    p <- runIOorExplode $ readMarkdown def s
    s <- runIOorExplode $ writeMarkdown (def { writerSetextHeaders = False }) $ enumerateBackwards p
    Data.Text.IO.writeFile "2es.md" s
