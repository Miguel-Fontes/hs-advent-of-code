-- file: delivery.hs
import Data.Maybe

-- Houses
data House = House ((Int, Int), Int) deriving (Show)

getX :: House -> Int
getX (House ((x, _),  _)) = x

getY :: House -> Int
getY (House ((_, y), _)) = y

getPresents :: House -> Int
getPresents (House ((_, _), n)) = n

buildHouse :: Int -> Int -> Int -> House
buildHouse x y p = House ((x, y), p)

-- Grid
data Grid = Grid [House] deriving (Show)

getCurrent :: Grid -> House
getCurrent (Grid xs) = head xs

addToGrid :: House -> Grid -> Grid
addToGrid h (Grid xs) = Grid $ h:xs

updateGrid :: (Int, Int) -> Grid -> Grid
updateGrid position g
    | wasVisited position g = addToGrid (buildHouse (fst destination) (snd destination) (presents + 1)) g
    | otherwise = addToGrid (buildHouse (fst destination) (snd destination) 0) g
        where pos = getCurrent g
              destination = (getX pos, inc $ getY pos)
              existingHousePresents = getPresents $ lookup' position g
              (presents) = existingHousePresents

wasVisited :: (Int, Int) -> Grid -> Bool
wasVisited (x, y) (Grid xs) = foldl (\acc h -> if getX h == x && getY h == y  then True else acc) False xs

lookup' :: (Int, Int) -> Grid -> Maybe House
lookup' (x, y) (Grid xs) = foldl (\acc h -> if getX h == x && getY h == y  then Just h else acc ) Nothing xs

removeFromGrid :: (Int, Int) -> Grid -> Grid
removeFromGrid (x, y) (Grid xs) = Grid . reverse $ foldl (\acc h -> if getX h == x && getY h == y  then acc else h:acc ) [] xs

moveUp :: Grid -> Grid
moveUp g
    | wasVisited destination g = g
    | otherwise = addToGrid (buildHouse (fst destination) (snd destination) 0) g
    where pos = getCurrent g
          destination = (getX pos, inc $ getY pos)

--moveTo :: Char -> Grid -> Grid
--moveTo x g
--    | x == '^' = addToGrid (buildHouse (getX current) (inc $ getY current) (inc $ getPresents current)) g
--    | x == '<' = addToGrid (buildHouse (dec $ getX current) (getY current) (inc $ getPresents current)) g
--    | x == 'v' = addToGrid (buildHouse (getX current) (dec $ getY current) (inc $ getPresents current)) g
--    | x == '>' = addToGrid (buildHouse (inc $ getX current) (getY current) (inc $ getPresents current)) g
--        where current = getCurrent g

inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x =  x - 1
