import Data.Char
import Text.Pandoc
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter f
    where
        f :: Inline -> Inline
        f (Str xs) = Str $ map toUpper xs
        f x = x
