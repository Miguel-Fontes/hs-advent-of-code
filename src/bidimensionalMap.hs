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

import qualified Data.Map.Strict as Map
import Control.Monad
import Data.List
import Data.Maybe

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
    | isJust (Map.lookup x xs) = getJust $ Map.lookup x xs
    | otherwise = []

updateRange :: Coordinates -> Coordinates -> (a -> a) -> Map.Map Int [(Int, a)] -> Map.Map Int [(Int, a)]
updateRange a b f m = Map.fromList $ foldl' (\acc x -> updateValue' x f acc) (Map.toList m) pointsToChange
    where pointsToChange = [ (x,y) | x <- [getX a..getX b], y <- [getY a..getY b] ]

updateValue' :: Coordinates -> (a -> a) -> [(Int, [(Int, a)])] -> [(Int, [(Int, a)])]
updateValue' (x,y) f = foldl' step []
    where step ks k
              | fst k == x = (fst k, updateRow y f (snd k)) : ks
              | otherwise = k : ks

updateValue :: Coordinates -> (a -> a) -> Map.Map Int [(Int, a)] -> Map.Map Int [(Int, a)]
updateValue (x,y) f m = Map.fromList $ Map.foldrWithKey step [] m
    where step k v ks
              | k == x = (k, updateRow y f v) : ks
              | otherwise = (k, v) : ks

updateRow :: Int -> (a -> a) -> [(Int, a)] -> [(Int, a)]
updateRow y f = foldl' step []
    where step acc x
              | fst x == y = (fst x, f $ snd x) : acc
              | otherwise  = x : acc

mapLength :: Map.Map Int [(Int, a)] -> Int
mapLength m = Map.size m * length (m Map.! 0)

mapCount :: (a -> Bool) -> Map.Map Int [(Int, a)] -> Int
mapCount p = Map.foldl' step 0
    where step ks v = innerStep v + ks
          innerStep = sum . map (\x -> if p $ snd x then 1 else 0)