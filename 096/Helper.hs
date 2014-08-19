module Helper where 

import Data.Map as Map

type Index = (Int,Int)
type Grid = Map.Map Index (Maybe Int)

allPairs :: [a] -> [b] -> [(a,b)]
allPairs x = zip (concat $ repeat x) . concatMap (replicate $ length x)

