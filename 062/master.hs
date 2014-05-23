import Data.List

int2list :: Integral a => a -> [a]
int2list = map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

digitsFromCubes :: Integral a => a -> [[a]]
digitsFromCubes n = map (sort . int2list . (^3)) [1..n]

numOcc :: Eq a => [a] -> a -> Int
numOcc l a = length $ filter (==a) l

run :: Integral a => a -> Int  -> a
run maxN x = head $ filter (\y -> numOcc l (sort $ int2list y) == x) $ map (^3) [1..maxN]
	where l = digitsFromCubes maxN

main = print $ run (10^4) 5
