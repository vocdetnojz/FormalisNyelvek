import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc
import qualified Data.Text.IO
import Data.Char (toUpper)

modHeader :: Block -> Block
modHeader (Header n _ xs) | n >= 3 = Para $ walk allCaps xs
modHeader x = x

allCaps :: Inline -> Inline
allCaps (Str xs) = Str $ map toUpper xs
allCaps x = x

changeHeaders :: Pandoc -> Pandoc
changeHeaders = walk modHeader

main :: IO ()
main = do
    s <- Data.Text.IO.readFile "example.md"
    p <- runIOorExplode $ readMarkdown def s
    s' <- runIOorExplode $ writeMarkdown (def { writerSetextHeaders = False }) $ changeHeaders p
    Data.Text.IO.writeFile "example2.md" s'
