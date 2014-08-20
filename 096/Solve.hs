module Solve where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Helper

allToChange :: Grid -> [Index]
allToChange = Map.keys . Map.filter(==Nothing)

indexesToVec :: Grid -> [Index] -> [Maybe Int]
indexesToVec g = map ((Map.!) g)

getRow :: Grid -> Index -> [Maybe Int]
getRow g (_,r) = indexesToVec g $ map (flip (,) r) [0..8]

getColumn :: Grid -> Index -> [Maybe Int]
getColumn g (c,_) = indexesToVec g $ map((,) c) [0..8]

getSquare :: Grid -> Index -> [Maybe Int]
getSquare g (c,r) = indexesToVec g $ zip (concat $ repeat [nc..nc+2]) $ concatMap (replicate 3) [nr..nr+2]
	where nc = (div c 3) * 3
	      nr = (div r 3) * 3


getOptions :: Grid -> Index -> [Int]
getOptions g i = [1..9] \\ (nub $ catMaybes $ concat [getRow g i, getColumn g i, getSquare g i])

solveGrid :: Grid -> Maybe Grid
solveGrid g  
	| null a = Just g
	| otherwise = helper g (head a) $ getOptions g $ head a
	where   a = allToChange g
		helper g i [] = Nothing
		helper g i (x:xs) = case (solveGrid $ Map.adjust (const $ Just x) i g) of
			Nothing -> helper g i xs
			Just g' -> Just g'

