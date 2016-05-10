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