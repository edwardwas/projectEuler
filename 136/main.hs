import Data.List

validPair :: (Int,Int) -> Bool
validPair (u,v) = 3*u > v && mod (u+v) 4 == 0 && mod (3*u - v) 4 == 0

genPairs :: Int -> [(Int,Int)]
genPairs n = concatMap (\v -> map ((,) v) [1 .. div n v] )  [ 1..n]

run :: Int -> Int -> Int
--Calculate all value of which there are m soultions below n
run m = length . filter ((== m) . length) . group . sort . 
		map (uncurry (*)) . filter validPair . genPairs 

main = print $ run 1 $ 10000
