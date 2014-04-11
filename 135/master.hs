
factorPairs :: Int -> [(Int,Int)]
factorPairs n = [(div n x,x) | x <- [1..n], mod n x == 0]

validPair :: (Int,Int) -> Bool
validPair (u,v) = and [ 3*u > v,mod (u+v) 4 == 0, mod (3*u - v) 4 == 0]

numSol :: Int -> Int
numSol n = length $ filter validPair $ factorPairs n

main = putStrLn $ show $ length $  filter (\x -> (numSol x) ==10) [1..10^6]

