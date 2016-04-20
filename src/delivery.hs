-- file: delivery.hs
import Data.Maybe
import Data.List

-- Day 3 --------------------------------------------------------------------------------------------------------------
-- Part One
delivery :: IO()
delivery = do
    input <- readFile "delivery-directions.txt"
    let startingPoint = makePoint (0, 0) (House 1)
        grid = makeGrid startingPoint
        presentsGrid = dropPresents startingPoint grid input
    putStrLn $ show (countValues presentsGrid)

-- Part Two
robotDelivery :: IO()
robotDelivery = do
    input <- readFile "delivery-directions.txt"
    let startingPoint = makePoint (0, 0) (House 1)
        grid = makeGrid startingPoint
        santaInput = splitDirections 0 input -- 0 para começar do zero
        robotInput = splitDirections 1 input -- 1 para começar do um
        santaPresentsGrid = dropPresents startingPoint grid santaInput
        robotPresentsGrid = dropPresents startingPoint grid robotInput
    putStrLn $ show ("Just like delivery but... with a Robot")
    putStrLn $ show ("Total houses visited: " ++ show (countValues $ mergeGrids santaPresentsGrid robotPresentsGrid))

-- Helpers
-- dropPresents => Executa a lógica de entregar os presentes de acordo com os caracteres
-- lidos do arquivo com as instruções de direções. Recebe um Point inicial para começar
-- a movimentação.
dropPresents :: Point House -> Grid House -> String -> Grid House
dropPresents _ g [] = g
dropPresents currPoint g (x:xs) = dropPresents destination updatedGrid xs
    where destination = makePoint (moveTo x (getCoords currPoint)) makeHouse
          updatedGrid = updateValue destination incPresents makeHouse g

-- moveTo => Faz a aritmética de movimentação dentro do Grid utilizando as tuplas
-- up = (x, y+1) | down = (x, y-1) | left = (x-1, y) | right = (x+1, y)
moveTo :: Char -> Coords -> Coords
moveTo x current
    | x == '^' = moveUp current
    | x == '<' = moveLeft current
    | x == 'v' = moveDown current
    | x == '>' = moveRight current

-- splitDirections => Splita uma string saltando índices de 2 em 2 à partir de um valor x
-- splitDirections 0 [1,2,3,4,5] == [1,3,5]
-- splitDirections 1 [1,2,3,4,5] == [2,4]
splitDirections :: Int -> String -> String
splitDirections n xs
              | n < length xs = (xs !! n) : splitDirections (n+2) (xs)
              | otherwise = []

-- Data Types ---------------------------------------------------------------------------------------------------------

-- Type: Houses
data House = House { presents :: Int } deriving (Show)

-- Operations
incPresents :: House -> House
incPresents (House p) = House (p + 1)

makeHouse :: House
makeHouse = House 0

-- Type Synonym: Coords -------------------------------------------------------------------------------------------------------
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

-- Type: Point --------------------------------------------------------------------------------------------------------
data Point a = Point (Coords, a) deriving (Show)

-- Typeclasses
instance Eq (Point a) where
    Point (a, _) == Point (b, _) = a == b

-- Constructor
makePoint :: Coords -> a -> Point a
makePoint (x, y) z = Point ((x,y), z)

-- Selectors
getCoords :: Point a -> Coords
getCoords (Point (coords, _)) = coords

getValue :: Point a -> a
getValue (Point (_, a)) = a


-- Type: Grid ---------------------------------------------------------------------------------------------------------
data Grid a = Grid [Point a] deriving (Show)

-- Construtor
makeGrid :: Point a -> Grid a
makeGrid point = Grid [point]

-- Operations
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

coordExists :: Point a -> Grid a -> Bool
coordExists point (Grid xs) = foldl (\acc x -> if x == point then True else acc) False xs

updateValue :: Point a -> (a -> a) -> a -> Grid a -> Grid a
updateValue point f valConstructor (Grid xs)
    | coordExists point (Grid xs) = Grid $ foldl (\acc x -> if x == point
                                                            then makePoint (getCoords point) (f $ getValue x) : acc
                                                            else x : acc) [] xs
    | otherwise = updateValue point f valConstructor (Grid $ makePoint (getCoords point) valConstructor : xs)