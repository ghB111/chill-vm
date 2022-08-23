module Parser ( parse
              , main
              , Bit (Zero, One)
              , Byte (Byte)
              , Parser.fromString ) where

import Prelude hiding (putStr, fromString)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)
import Data.List.Split (chunksOf)

one = '👍' -- does not need encoding
zero = '\129305' -- encoded '🤙'

preprocess :: String -> [String]
preprocess = (chunksOf 8) . (filter (\x -> x `elem` [one, zero]))

toBits :: [String] -> [String]
toBits = map (map (\x -> if x == one then '1' else '0'))

parse :: String -> [String]
parse = toBits . preprocess

main :: IO ()
main = interact $ unlines . preprocess

data Bit = Zero | One deriving (Eq)
data Byte = Byte (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
    deriving (Eq)

instance Show Bit where
    show Zero = "0"
    show One = "1"

instance Show Byte where
    show (Byte (b1, b2, b3, b4, b5, b6, b7, b8)) = foldl (++) "" $ map show bits
        where bits = [b1, b2, b3, b4, b5, b6, b7, b8]

fromChar :: Char -> Bit
fromChar '0' = Zero
fromChar '1' = One

fromString :: String -> Byte
fromString str@[ n1
           , n2
           , n3
           , n4
           , n5
           , n6
           , n7
           , n8 ] = Byte (b1, b2, b3, b4, b5, b6, b7, b8)
                      where [b1, b2, b3, b4, b5, b6, b7, b8] = map fromChar str


