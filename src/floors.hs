-- file: floors.hs

floors :: String -> Int
floors [] = 0
floors (x:xs) = move x + floors xs

hitTheBasement :: String -> Int
hitTheBasement x = floorsIter x 0 0
    where floorsIter [] _ i = i
          floorsIter (x:xs) n i
              | n == (-1) = i
              | otherwise = floorsIter xs (n + move x) (i+1)

move :: Char -> Int
move x
    | x == up = 1
    | x == down = (-1)
    | otherwise = 0
        where up = '('
              down = ')'