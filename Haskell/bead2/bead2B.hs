import Data.Char
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.JSON

behead :: Block -> Block
behead (Header n a xs) = Header (n+1) a xs
behead x = x

main :: IO ()
main = toJSONFilter behead
