-- file: floors.hs

floors :: String -> Int
floors [] = 0
floors (x:xs) = move x + floors xs

hitTheBasement :: String -> Int -> Int -> Int
hitTheBasement [] _ i = i
hitTheBasement (x:xs) n i
    | n == (-1) = i
    | otherwise = hitTheBasement xs f (i+1)
        where f = n + move x

move :: Char -> Int
move x
    | x == up = 1
    | x == down = (-1)
    | otherwise = 0
        where up = '('
              down = ')'
