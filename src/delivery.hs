-- file: delivery.hs
import Data.Maybe
import Data.List

-- Houses ---------------------------------------------------------------------------------
data House = House { presents :: Int } deriving (Show)

incPresents :: House -> House
incPresents (House p) = House (p + 1)

makeHouse :: House
makeHouse = House 0

-- Coords ---------------------------------------------------------------------------------
type Coords = (Int, Int)

-- Construtor
makeCoords :: Int -> Int -> Coords
makeCoords x y = (x,y)

-- Selectors
getX :: Coords -> Int
getX coords = fst coords

getY :: Coords -> Int
getY coords = snd coords

-- Operations
incX :: Coords -> Coords
incX coords = (getX coords + 1, getY coords)

decX :: Coords -> Coords
decX coords = (getX coords - 1, getY coords)

incY :: Coords -> Coords
incY coords = (getX coords, getY coords + 1)

decY :: Coords -> Coords
decY coords = (getX coords, getY coords - 1)

-- Point ----------------------------------------------------------------------------------
data Point a = Point (Coords, a) deriving (Show)

instance Eq (Point a) where
    Point (a, _) == Point (b, _) = a == b

makePoint :: Coords -> a -> Point a
makePoint (x, y) z = Point ((x,y), z)

getCoords :: Point a -> Coords
getCoords (Point (coords, _)) = coords

getValue :: Point a -> a
getValue (Point (_, a)) = a


-- Grid -----------------------------------------------------------------------------------
data Grid a = Grid [Point a] deriving (Show)

-- Construtor
makeGrid :: Point a -> Grid a
makeGrid point = Grid [point]


coordExists :: Point a -> Grid a -> Bool
coordExists point (Grid xs) = foldl (\acc x -> if x == point then True else acc) False xs

updateValue :: Point a -> (a -> a) -> a -> Grid a -> Grid a
updateValue point f valConstructor (Grid xs)
    | coordExists point (Grid xs) = Grid $ foldl (\acc x -> if x == point
                                                            then makePoint (getCoords point) (f $ getValue x) : acc
                                                            else x : acc) [] xs
    | otherwise = updateValue point f valConstructor (Grid $ makePoint (getCoords point) valConstructor : xs)

moveUp :: Coords -> Coords
moveUp coords = incY coords

moveRight :: Coords -> Coords
moveRight coords = incX coords

moveDown :: Coords -> Coords
moveDown coords = decY coords

moveLeft :: Coords -> Coords
moveLeft coords = decX coords

countValues :: Grid a -> Int
countValues (Grid xs) = length xs

mergeGrids :: Grid a -> Grid a -> Grid a
mergeGrids (Grid ga) (Grid gb) = Grid $ union ga gb

-- Day 3
delivery :: IO()
delivery = do
    input <- readFile "delivery-directions.txt"
    let startingPoint = makePoint (0, 0) (House 1)
        grid = makeGrid startingPoint
        presentsGrid = dropPresents startingPoint grid input
    putStrLn $ show (countValues presentsGrid)

-- Day 3 - Part Two
robotDelivery :: IO()
robotDelivery = do
    input <- readFile "delivery-directions.txt"
    let startingPoint = makePoint (0, 0) (House 1)
        grid = makeGrid startingPoint
        santaInput = splitDirections input 0 -- 0 para começar dos pares
        robotInput = splitDirections input 1 -- 1 para começar dos ímpares
        santaPresentsGrid = dropPresents startingPoint grid santaInput
        robotPresentsGrid = dropPresents startingPoint grid robotInput
    putStrLn $ show ("Santa Input: " ++ (show $ length santaInput))
    putStrLn $ show ("Robot Input: " ++ (show $ length robotInput))
    putStrLn $ show ("Total Input: " ++ (show $ length input))
    putStrLn $ show (countValues santaPresentsGrid)
    putStrLn $ show (countValues robotPresentsGrid)
    putStrLn $ show (countValues robotPresentsGrid + countValues santaPresentsGrid)
    putStrLn $ show (countValues $ mergeGrids santaPresentsGrid robotPresentsGrid)

dropPresents :: Point House -> Grid House -> String -> Grid House
dropPresents _ g [] = g
dropPresents currPoint g (x:xs) = dropPresents destination updatedGrid xs
    where destination = makePoint (moveTo x (getCoords currPoint)) makeHouse
          updatedGrid = updateValue destination incPresents makeHouse g

moveTo :: Char -> Coords -> Coords
moveTo x current
    | x == '^' = moveUp current
    | x == '<' = moveLeft current
    | x == 'v' = moveDown current
    | x == '>' = moveRight current

splitDirections :: String -> Int -> String
splitDirections (x:xs) n = x : splitIter n xs
    where splitIter _ [] = []
          splitIter n (x:xs) = if even n then splitIter (n+1) xs else x : splitIter (n+1) xs