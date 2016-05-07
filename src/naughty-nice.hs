import Control.Monad
import Data.List

atLeast3Vowels :: String -> Bool
atLeast3Vowels xs
    | numberOfVowels >= 3 = True
    | otherwise = False
    where vowels = ['a', 'e', 'i', 'o', 'u']
          numberOfVowels = length $ filter (\x -> x `elem` vowels) xs

containRepeatedLetter :: String -> Bool
containRepeatedLetter [x] = False
containRepeatedLetter (x:xs)
    | x == head xs = True
    | otherwise = containRepeatedLetter xs

noNastyWords :: String -> Bool
noNastyWords xs = not $ any (\nw -> nw `isInfixOf` xs) nastyWords
    where nastyWords = ["ab", "cd", "pq", "xy"]

nonOverlappingPairs :: String -> Bool
nonOverlappingPairs [x] = False
nonOverlappingPairs (x:y:[]) = False
nonOverlappingPairs (x:y:xs)
    | [x,y] `elem` xsPairs = True
    | otherwise = nonOverlappingPairs (y:xs)
    where xsPairs = breakEach 2 xs

intercalatedRepetition :: String -> Bool
intercalatedRepetition (x:_:y:[]) = x == y
intercalatedRepetition (x:z:y:xs) | x == y = True | otherwise = intercalatedRepetition (z:y:xs)
intercalatedRepetition _ = False

breakEach :: Int -> [a] -> [[a]]
breakEach _ [] = []
breakEach _ [x] = []
breakEach n (x:y:xs) = [x,y] : breakEach n (y:xs)

main :: IO()
main = do
    strings <- liftM lines (readFile "naughty-nice-input.txt")
    let criterias = [atLeast3Vowels, containRepeatedLetter, noNastyWords]
    let criterias2 = [nonOverlappingPairs, intercalatedRepetition]
    result <- return $ strings >>= (\s -> [all (\c -> c s) criterias])
    result2 <- return $ strings >>= (\s -> [all (\c -> c s) criterias2])
    putStrLn "--- Part One -----------------------------------"
    putStrLn $ "Total messages: " ++ show (length result)
    putStrLn $ "Naughty ones: " ++ show (length $ filter (==False) result)
    putStrLn $ "Nice ones: " ++ show (length $ filter (==True) result)
    putStrLn "--- Part Two -----------------------------------"
    putStrLn $ "Total messages: " ++ show (length result2)
    putStrLn $ "Naughty ones: " ++ show (length $ filter (==False) result2)
    putStrLn $ "Nice ones: " ++ show (length $ filter (==True) result2)