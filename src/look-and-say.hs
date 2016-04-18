-- file: look-and-say.hs

lookAndSay :: Int -> String -> (Int, String)
lookAndSay 0 x = (length x, x)
lookAndSay n x = lookAndSay (n-1) (recite x)

recite :: String -> String
recite [] = []
recite xs = show n ++ x : recite (drop n xs)
  where n = length $ takeWhile (==x) xs
        x = head xs