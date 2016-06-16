module Matchsticks where

import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B

main = do
    file <- fmap (map (\x -> (memoryLength x, literalLength x)) . lines) (readFile "Matchsticks-input.txt")
    let uzp = unzip file
        mem = sum $ fst uzp
        lit = sum $ snd uzp
    print (take 1 file)
    print (mem - lit)


memoryLength :: String -> Int
memoryLength = length

literalLength :: String -> Int
literalLength = length . removeScapes

removeScapes :: String -> String
removeScapes [] = []
removeScapes (x:xs)
    | x == '\\' = case head xs of
                    '\\' -> '\\' : removeScapes (tail xs)
                    'x' -> 'x' : removeScapes (drop 3 xs)
                    '"' -> '"' : removeScapes (tail xs)
                    x -> x : removeScapes (drop 1 xs)
    | otherwise = x : removeScapes xs