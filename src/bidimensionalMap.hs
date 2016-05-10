module BidimensionalMap
    (
         makeMapGrid
        ,getValue
        ,getRow
        ,updateRow
        ,updateValue
        ,updateRange
        ,mapLength
        ,mapCount
    )
where

import qualified Data.Map as Map
import Control.Monad
import Data.List

type Dimensions = (Int, Int)

-----------------------------------------------------------------------------------------------------------------------
type Coordinates = (Int, Int)
getX :: Coordinates -> Int
getX (x, _) = x

getY :: Coordinates -> Int
getY (_, y) = y
-----------------------------------------------------------------------------------------------------------------------

makeMapGrid :: Dimensions -> a -> Map.Map Int [(Int, a)]
makeMapGrid (maxX, maxY) t = Map.fromList [ (x, row) | x <- [0..maxX-1] ]
    where row = [ (y,t) | y <- [0..maxY-1] ]

getValue :: Coordinates -> Map.Map Int [(Int, a)] -> Maybe a
getValue (x,y) xs = join $ Map.lookup y . Map.fromList <$> Map.lookup x xs

getJust :: Maybe a -> a
getJust (Just a) = a

getRow :: Eq a => Int -> Map.Map Int [(Int, a)] -> [(Int, a)]
getRow x xs
    | Map.lookup x xs /= Nothing = getJust $ Map.lookup x xs
    | otherwise = []

updateRange :: Coordinates -> Coordinates -> (a -> a) -> Map.Map Int [(Int, a)] -> Map.Map Int [(Int, a)]
updateRange a b f m = foldl' (\acc x -> updateValue x f acc) m pointsToChange
    where pointsToChange = [ (x,y) | x <- [getX a..getX b], y <- [getY a..getY b] ]


updateValue :: Coordinates -> (a -> a) -> Map.Map Int [(Int, a)] -> Map.Map Int [(Int, a)]
updateValue (x,y) f m = Map.fromList $ Map.foldrWithKey step [] m
    where step k v ks
              | k == x = (k, updateRow y f v) : ks
              | otherwise = (k, v) : ks

updateRow :: Int -> (a -> a) -> [(Int, a)] -> [(Int, a)]
updateRow y f = foldr step []
    where step x acc
              | fst x == y = (fst x, f $ snd x) : acc
              | otherwise  = x : acc

mapLength :: Map.Map Int [(Int, a)] -> Int
mapLength m = Map.size m * (length $ m Map.! 0)

mapCount :: (a -> Bool) -> Map.Map Int [(Int, a)] -> Int
mapCount p m = Map.foldr step 0 m
    where step v ks = innerStep v + ks
          innerStep = sum . map (\x -> if p $ snd x then 1 else 0)
