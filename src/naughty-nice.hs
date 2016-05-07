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
nonOverlappingPairs xs = False

intercalateRepetition :: String -> Bool
intercalateRepetition xs = False

{-
    It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).

    It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
-}

main :: IO()
main = do
    strings <- liftM lines (readFile "naughty-nice-input.txt")
    let criterias = [atLeast3Vowels, containRepeatedLetter, noNastyWords]
    let criterias2 = [nonOverlappingPairs, intercalateRepetition]
    result <- return $ strings >>= (\s -> [all (\c -> c s) criterias])
    result2 <- return $ strings >>= (\s -> [all (\c -> c s) criterias2])
    putStrLn "--- Part One ------------------------------------------------------------"
    putStrLn $ "Total messages: " ++ show (length result)
    putStrLn $ "Naughty ones: " ++ show (length $ filter (==False) result)
    putStrLn $ "Nice ones: " ++ show (length $ filter (==True) result)
    putStrLn "--- Part Two ------------------------------------------------------------"
    putStrLn $ "Total messages: " ++ show (length result2)
    putStrLn $ "Naughty ones: " ++ show (length $ filter (==False) result2)
    putStrLn $ "Nice ones: " ++ show (length $ filter (==True) result2)