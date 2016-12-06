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
       (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['!', '@']) !! c

hexByte2Chr :: String -> Char
hexByte2Chr c =
    chr $ hex2Int c

hex2Str :: String -> String
hex2Str "" = ""
hex2Str s =
    hexByte2Chr (take 2 s) : hex2Str (drop 2 s)

ints2Bytes :: [Int] -> Int
ints2Bytes m =
    shiftL (m !! 0) 16 + shiftL (m !! 1) 8 + m !! 2

bytes2Base64 :: Int -> String
bytes2Base64 0 = ""
bytes2Base64 n =
    (bytes2Base64 $ shiftR n 6) ++ [int2Sym $ n .&. 63]

ints2Base64 :: [Int] -> String
ints2Base64 m =
    (take n $ bytes2Base64 $ ints2Bytes $ m ++ replicate (3 - length m) 0) ++ replicate (4 - n) '=' where n = div ((length m) * 4) 3

chrs2Base64 :: String -> String
chrs2Base64 s =
    ints2Base64 $ map ord s

str2Base64 :: String -> String
str2Base64 "" = ""
str2Base64 s =
    chrs2Base64 (take 3 s) ++ str2Base64 (drop 3 s)

main = do
    line <- getLine
    putStrLn $ hex2Str line
    putStrLn $ str2Base64 $ hex2Str line
