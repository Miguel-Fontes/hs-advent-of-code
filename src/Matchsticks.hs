module Matchsticks where

--- Parte 1 -----------------------------------------------------------------------------
--  Resposta 1371
main = do
    file <- readFile "Matchsticks-input.txt"

    let result =  (sum . map memToLiteralDiff . lines) file
    let result2 =  (sum . map memToQuotedLiteralDiff . lines) file

    putStrLn $ "Part One: " ++ show result
    putStrLn $ "Part Two: " ++ show result2

memToLiteralDiff :: String -> Int
memToLiteralDiff s = memoryLength s - literalLength s

memToQuotedLiteralDiff :: String -> Int
memToQuotedLiteralDiff s = quotedLiteralLength s - literalLength s

memoryLength :: String -> Int
memoryLength = (addQuotes . length)
    where addQuotes = (+2)

quotedLiteralLength :: String -> Int
quotedLiteralLength = addQuotes . literalLength
    where addQuotes = (+2)

literalLength :: String -> Int
literalLength [] = 0
literalLength (x:xs)
    | x == '\\' = case head xs of
                      '\\' -> 1 + literalLength (tail xs)
                      'x' -> 1 + literalLength (drop 3 xs)
                      '"' -> 1 + literalLength (tail xs)
    | otherwise = 1 + literalLength xs

