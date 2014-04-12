import Data.List

validPair :: (Int,Int) -> Bool
validPair (u,v) = and [ 3*u > v,mod (u+v) 4 == 0, mod (3*u - v) 4 == 0]

multPair :: (Int,Int) -> Int
multPair (u,v) = u*v

isDivisable :: Int -> Int -> Bool
isDivisable n x = mod n x == 0

genPairs :: Int -> [(Int,Int)]
genPairs n = concatMap (\v -> map (\u -> (v,u)) [1.. div n v] )  [ 1..n]

run :: Int -> Int -> Int
--Calculate all value of which there are m soultions below n
run n m = length $ filter (\x -> length x == m) $  groupedVals n
        where groupedVals n = group $ sort $ map multPair $ filter validPair $ genPairs n

main = putStrLn $ show $ run (10^6) 10
