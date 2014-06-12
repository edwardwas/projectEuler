import Data.List

isFactor :: Integral a => a -> a -> Bool
isFactor x y = mod x y == 0

pairFactors :: Integral a => a -> [(a,a)]
pairFactors n = map (\x -> (x, div n x)) $ filter (isFactor n) $ [1..n]

numSol = length . filter filterPair . pairFactors 
	where filterPair (u,v) = and [isFactor (3*v-u) 4, isFactor (u+v) 4, 3*v > u]

run target n = length $ filter (==target) $ map numSol [1..n]

main = print $ run 1 $ 50*10^6-1

