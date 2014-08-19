module Solve where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Helper

coordList :: [Index]
coordList = zip (concat $ repeat [0..8]) $ concatMap (replicate 9) [0..8]

isValidVec :: [Maybe Int] -> Bool
isValidVec = (==) [1..9] . sort . catMaybes

indexesToVec :: Grid -> [Index] -> [Maybe Int]
indexesToVec g = map ((Map.!) g)

getRow :: Grid -> Index -> [Maybe Int]
getRow g (_,r) = indexesToVec g $ map (\x -> (x,r)) [0..8]

getColumn :: Grid -> Index -> [Maybe Int]
getColumn g (c,_) = indexesToVec g $ map(\x -> (c,x)) [0..8]

getSquare :: Grid -> Index -> [Maybe Int]
getSquare g (c,r) = indexesToVec g $ zip (concat $ repeat [nc..nc+2])
			 $ concatMap (replicate 3) [nr..nr+2]
	where nc = (div c 3) * 3
	      nr = (div r 3) * 3


getOptions :: Grid -> Index -> [Int]
getOptions g i = [1..9] \\ (nub $ catMaybes $ concat [getRow g i, getColumn g i, getSquare g i])

isSolved :: Grid -> Bool
isSolved = (==) Nothing . find (==Nothing) . map (snd) . Map.toDescList

solveGrid :: Grid -> Maybe Grid
solveGrid g = helper g $ coordList 
	where helper g [] = Just g
	      helper g (x:xs)
			| g Map.! x == Nothing = helper2 g x $ getOptions g x
			| otherwise = helper g xs
	      helper2 :: Grid -> Index -> [Int] ->  Maybe Grid
	      helper2 g i [] = Nothing
	      helper2 g i (x:xs) = case (solveGrid $ Map.adjust (const$ Just x) i g) of
		Nothing -> helper2 g i xs
		Just g' -> Just g'

