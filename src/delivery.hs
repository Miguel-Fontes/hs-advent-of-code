-- file: delivery.hs
import Data.Maybe

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

-- Grid -----------------------------------------------------------------------------------
data Grid a = Grid [(Coords, a)] deriving (Show)

-- Construtor
makeGrid :: Coords -> a -> Grid a
makeGrid coords x = Grid [(coords, x)]

-- Selectors
getCoords :: (Coords, a) -> Coords
getCoords (coords, _) = coords

getValue :: (Coords, a) -> a
getValue (_, a) = a

-- Operations
coordExists :: Coords -> Grid a -> Bool
coordExists pos (Grid xs) = if isNothing (lookup pos xs)  then False else True

updateValue :: Coords -> (a -> a) -> a -> Grid a -> Grid a
updateValue coords f valueBuilder (Grid xs)
    | coordExists coords (Grid xs) = Grid $ foldl (\acc x -> if getCoords x == coords
                                                   then (getCoords x, f $ getValue x) : acc
                                                   else x : acc) [] xs
    | otherwise = updateValue coords f valueBuilder (Grid $ (coords, valueBuilder) : xs)

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

-- Day 3
delivery :: IO()
delivery = do
    input <- readFile "delivery-directions.txt"
    let grid = makeGrid (0, 0) (House 1)
        current = makeCoods 0 0
        presentsGrid = dropPresents current grid input
    putStrLn $ show (countValues presentsGrid)

dropPresents :: Coords -> Grid House -> String -> Grid House
dropPresents _ g [] = g
dropPresents coords g (x:xs) = dropPresents destination (updateValue destination incPresents makeHouse g) xs
    where destination = moveTo x coords

moveTo :: Char -> Coords -> Coords
moveTo x current
    | x == '^' = moveUp current
    | x == '<' = moveLeft current
    | x == 'v' = moveDown current
    | x == '>' = moveRight current