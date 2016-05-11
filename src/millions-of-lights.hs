import Control.Monad
import Data.List
import qualified Data.Map as Map
import BidimensionalMap

-----------------------------------------------------------------------------------------------------------------------
data Light = On | Off deriving (Show, Eq)

toggleLight, turnLightOn, turnLightOff :: Light -> Light
toggleLight On = Off
toggleLight Off = On

turnLightOn _ = On

turnLightOff _ = Off

-----------------------------------------------------------------------------------------------------------------------
data Instruction = Instruction String (Int, Int) (Int, Int)
                 | Operation String
                 | Coord (Int, Int)
                 | Empty
                   deriving (Read, Show)

processInput :: String -> [Instruction]
processInput = map (makeInstruction . parseData . unwords . filter (/="through") . words ) . lines

makeInstruction :: [Instruction] -> Instruction
makeInstruction ((Operation op):(Coord x):(Coord y):[]) = Instruction op x y

parseData :: String -> [Instruction]
parseData [] = []
parseData (x:xs)
    | x `elem` ['0'..'9'] = let fileCoords = x : takeWhile (/=' ') xs
                                strCoords = splitStringAt ',' fileCoords
                            in  Coord (read (strCoords !! 0) :: Int, read (strCoords !! 1) :: Int )
                                : parseData (drop (length fileCoords) xs)
    | x `elem` ['a'..'z'] = let strOperation = rtrim $ (x : (takeWhile (\z -> not $ z `elem` ['0'..'9']) xs))
                            in  Operation strOperation : parseData (drop (length strOperation) xs)
    | x == ' ' = parseData xs
    | x == ',' = parseData xs
    | otherwise = Empty : parseData []


executeInstructions :: [Instruction] -> Map.Map Int [(Int, Light)] -> Map.Map Int [(Int, Light)]
executeInstructions [] g = g
executeInstructions ((Instruction op x y):xs) g
    | op == "turn on" = executeInstructions xs (updateRange x y turnLightOn g)
    | op == "turn off" = executeInstructions xs (updateRange x y turnLightOff g)
    | op == "toggle" = executeInstructions xs (updateRange x y toggleLight g)
    | otherwise = error "Invalid instruction!"

main :: IO()
main = do
    contents <- readFile "millions-of-lights-instructions.txt"
    instructions <- return (processInput contents)
    let grid = makeMapGrid (1000,1000) Off
        resultGrid = executeInstructions (take 1 instructions) grid
        --resultGrid = executeInstructions instructions grid
        onLights = mapCount (==On) resultGrid
    putStrLn $ show onLights

-- Helpers -- Separar em módulo
splitStringAt :: Char -> String -> [String]
splitStringAt p xs =
    let break c
            | c == p = ' '
            | otherwise = c
    in words $ map break xs

rtrim :: String -> String
rtrim [] = []
rtrim xs
    | last xs == ' ' = rtrim (reverse $ drop 1 (reverse xs))
    | otherwise = xs

trim :: String -> String
trim = reverse . foldr (\x acc -> if x == ' ' then acc else x : acc) []