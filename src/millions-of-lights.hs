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

turnOn :: Coordinates -> Coordinates -> Grid Light -> Grid Light
turnOn a b (Grid g) = Grid $ union [ Point (x,y) On | x <- [getX a..getX b], y <- [getY a..getY b] ] g

turnOff :: Coordinates -> Coordinates -> Grid Light -> Grid Light
turnOff a b (Grid g) = Grid $ union [ Point (x,y) Off | x <- [getX a..getX b], y <- [getY a..getY b] ] g

toggle :: Coordinates -> Coordinates -> Grid Light -> Grid Light
toggle a b (Grid xs) = Grid $ foldl (\acc x -> if (getCoords x) `elem` pointsToToggle
                                               then Point (getCoords x) (toggleLight $ getValue x) : acc
                                               else x : acc ) [] xs
    where pointsToToggle = [ (x,y) | x <- [getX a..getX b], y <- [getY a..getY b] ]

gridLength :: Grid a -> Int
gridLength (Grid g) = length g

gridMap :: (Point a -> Point a) -> Grid a -> Grid a
gridMap f (Grid g) = Grid $ map f g

gridFilter :: (Point a -> Bool) -> Grid a -> Grid a
gridFilter f (Grid g) = Grid $ filter f g

-----------------------------------------------------------------------------------------------------------------------
data Light = On | Off deriving (Show, Eq)

toggleLight :: Light -> Light
toggleLight On = Off
toggleLight Off = On

-----------------------------------------------------------------------------------------------------------------------
data Point a = Point (Int, Int) a deriving (Show)

instance Eq (Point a) where
    Point x _ == Point y _ = x == y

getCoords :: Point a -> Coordinates
getCoords (Point coords _) = coords

getValue :: Point a -> a
getValue (Point _ a) = a

-----------------------------------------------------------------------------------------------------------------------
processInput :: [String] -> [String]
processInput = map (unwords . filter (/="through") . words)

dispatcher :: [String]
dispatcher = undefined

main :: IO()
main = do
    contents <- liftM lines $ readFile "millions-of-lights-instructions.txt"
    putStrLn $ show (processInput $ take 1 contents)