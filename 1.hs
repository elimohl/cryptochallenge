import Text.Printf
import Text.Read
import Data.List
import Data.Word
import Data.Char
import Data.Bits
--import Data.ByteString

just2Int :: Maybe Int -> Int 
just2Int (Just n) = n
just2Int Nothing = 0

hexSym2Int :: Char -> Int 
hexSym2Int c =
    just2Int $ elemIndex c (['0'..'9'] ++ ['a'..'f'])

hex2Int :: String -> Int 
hex2Int s =
     foldl (\a b -> a * 16  + b) 0 $ map hexSym2Int s

int2Sym :: Int -> Char
int2Sym c =
       (['A'..'z'] ++ ['0'..'9'] ++ ['!', '@']) !! c

hexByte2Chr :: String -> Char
hexByte2Chr c =
    chr $ hex2Int c

hex2Str :: String -> String
hex2Str "" = ""
hex2Str s =
    hexByte2Chr (take 2 s) : hex2Str (drop 2 s)

main = do
    line <- getLine
    putStrLn $ hex2Str line
