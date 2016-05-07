import qualified Data.Digest.Pure.MD5 as M
import qualified Data.ByteString.Lazy.Char8 as B

sequenceInput :: String -> [String]
sequenceInput seed = map (\x -> seed ++ show x) [1..]

md5Gen :: String -> String
md5Gen = show . M.md5 . B.pack

main :: IO()
main = do
    pattern <- putStrLn "Insert preffix for MD5 Hash Search" >> getLine
    let inputList = sequenceInput "bgvyzdsv"
        result = head $ dropWhile (\x -> (take (length pattern) $ md5Gen x) /= pattern) inputList
    putStrLn $ show result