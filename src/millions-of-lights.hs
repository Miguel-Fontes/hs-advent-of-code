import Control.Monad
import Data.List

-----------------------------------------------------------------------------------------------------------------------
type Coordinates = (Int, Int)
getX :: Coordinates -> Int
getX (x, _) = x

getY :: Coordinates -> Int
getY (_, y) = y

-----------------------------------------------------------------------------------------------------------------------
data Grid a = Grid [Point a] deriving (Show, Eq)

type Dimensions = (Int, Int)

makeGrid :: Dimensions -> a -> Grid a
makeGrid (maxX, maxY) t = Grid [ Point (x,y) t | x <- [0..maxX-1], y <- [0..maxY-1] ]

updatePoint :: Coordinates -> Coordinates -> (Light -> Light) -> Grid Light -> Grid Light
updatePoint a b f (Grid xs) = Grid $ foldl (\acc x -> if (getCoords x) `elem` pointsToToggle
                                               then Point (getCoords x) (f $ getValue x) : acc
                                               else x : acc ) [] xs
    where pointsToToggle = [ (x,y) | x <- [getX a..getX b], y <- [getY a..getY b] ]

gridLength :: Grid a -> Int
gridLength (Grid g) = length g

gridMap :: (Point a -> Point a) -> Grid a -> Grid a
gridMap f (Grid g) = Grid $ map f g

gridFilter :: (Point a -> Bool) -> Grid a -> Grid a
gridFilter f (Grid g) = Grid $ filter f g

-----------------------------------------------------------------------------------------------------------------------
data Point a = Point (Int, Int) a deriving (Show)

instance Eq (Point a) where
    Point x _ == Point y _ = x == y

getCoords :: Point a -> Coordinates
getCoords (Point coords _) = coords

getValue :: Point a -> a
getValue (Point _ a) = a

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


executeInstructions :: [Instruction] -> Grid Light -> Grid Light
executeInstructions [] g = g
executeInstructions ((Instruction op x y):xs) g
    | op == "turn on" = executeInstructions xs (updatePoint x y turnLightOn g)
    | op == "turn off" = executeInstructions xs (updatePoint x y turnLightOff g)
    | op == "toggle" = executeInstructions xs (updatePoint x y toggleLight g)
    | otherwise = error "Invalid instruction!"

main :: IO()
main = do
    contents <- readFile "millions-of-lights-instructions.txt"
    instructions <- return (processInput contents)
    let grid = makeGrid (100,100) Off
        resultGrid = executeInstructions (take 10 instructions) grid
        onLights = gridLength $ gridFilter (\x -> (getValue x) == On) resultGrid
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