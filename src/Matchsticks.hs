module Matchsticks where

main = do
    file <- readFile "Matchsticks-input.txt"

    let result =  (sum . map memToLiteralDiff . lines) file

    print $ (sum . map memToLiteralDiff . lines) file

memToLiteralDiff :: String -> Int
memToLiteralDiff s = memoryLength s - literalLength s

memoryLength :: String -> Int
memoryLength = ((+2) . length)

literalLength :: String -> Int
literalLength [] = 0
literalLength (x:xs)
    | x == '\\' = case head xs of
                      '\\' -> 1 + literalLength (tail xs)
                      'x' -> 1 + literalLength (drop 3 xs)
                      '"' -> 1 + literalLength (tail xs)
    | otherwise = 1 + literalLength xs