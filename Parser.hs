module Parser (parse, main) where

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr)
import Data.ByteString.UTF8 (fromString)
import Data.List.Split (chunksOf)

one = '👍' -- does not need encoding
zero = '\129305' -- encoded '🤙'

parse :: String -> String
parse = unlines . (chunksOf 8) . (filter (\x -> x `elem` [one, zero]))

main :: IO ()
main = interact parse

