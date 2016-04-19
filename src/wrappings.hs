-- file: wrappings.hs

wrappingList :: IO ()
wrappingList = do
    contents <- readFile "wrappings-list.txt"
    let list = map (parseStrListToInt . splitStringAt 'x') $ lines contents
        totalWrapping = sum $ map wrappingSize list
    putStrLn $ show totalWrapping

wrappingSize :: [Int] -> Int
wrappingSize [l, w, h] = (+extra) . sum $ map (*2) areas
    where areas = [l*w, w*h, h*l]
          extra = minimum areas

parseStrToInt :: String -> Int
parseStrToInt = read

parseStrListToInt :: [String] -> [Int]
parseStrListToInt = map parseStrToInt

splitStringAt :: Char -> String -> [String]
splitStringAt p xs =
    let break c
            | c == p = ' '
            | otherwise = c
    in words $ map break xs