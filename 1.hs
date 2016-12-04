import Text.Printf
import Text.Read
import Data.List
import Data.Word
import Data.Char
--import Data.ByteString

just2Int (Just n) = n
just2Int Nothing = 0

sym2Int (c) =
    just2Int $ elemIndex c (['0'..'9'] ++ ['a'..'z'])

hex2Dec (s) =
     foldl (\a b -> a * 16  + b) 0 $ map sym2Int s

int2Sym (c) =
       (['A'..'z'] ++ ['0'..'9'] ++ ['!', '@']) !! c

hex2Chr (c) =
    chr $ hex2Dec c

hex2Str "" = ""
hex2Str (s) =
    hex2Chr (take 2 s) : hex2Str (drop 2 s)

main = do
    line <- getLine
    putStrLn $ hex2Str line
