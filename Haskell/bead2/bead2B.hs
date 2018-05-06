import Data.Char
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.JSON


getIntFromMetaValue :: MetaValue -> Int
getIntFromMetaValue (MetaString s) = read s::Int
getIntFromMetaValue _ = 0

getIntFromMeta :: String -> Meta -> Int
getIntFromMeta s m = getIntFromMetaValue (fromMeta (MetaBool True) (lookupMeta s m))


behead :: Block -> Int -> Block
behead (Header n a xs) inc = Header (n+inc) a xs
behead x _ = x

runner :: Pandoc -> Pandoc
runner (Pandoc m b) = Pandoc m (map (\x -> (behead x (getIntFromMeta "liftheader" m))) b)
--runner x = x

main :: IO ()
main = toJSONFilter runner
