-- file: wrappings.hs

wrappingList :: IO ()
wrappingList = do
    contents <- readFile "wrappings-list.txt"
    let list = map (parseStrListToInt . splitStringAt 'x') $ lines contents
        totalWrapping = sum $ map wrappingSize list
        totalRibbon = sum $ map ribbonSize list
    putStrLn $ "Total Wrapping: " ++ show totalWrapping
    putStrLn $ "Total Ribbon:   " ++ show totalRibbon

wrappingSize :: [Int] -> Int
wrappingSize [l, w, h] = (+extra) . sum $ map (*2) areas
    where areas = [l*w, w*h, h*l]
          extra = minimum areas

ribbonSize :: [Int] -> Int
ribbonSize [l, w, h] = volume + perimeter
    where perimeter = minimum [l+w, w+h, h+l] * 2
          volume = l * w * h

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