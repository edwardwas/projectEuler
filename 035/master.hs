import qualified Data.Numbers.Primes as Primes

digSum :: Integral a => a -> [a]
digSum = map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

allRot :: (Eq a) => [a] -> [[a]]
allRot l = l : (helper $ (tail l) ++ [head l])
	where helper (x:xs) = if (x:xs) == l then [] else [x:xs] ++ helper (xs ++ [x])

list2num :: Integral a => [a] -> a
list2num = sum . zipWith (*) (iterate (*10) 1)

digitFilter :: Integral a => [a] -> Bool
digitFilter = all (\x -> elem x [1,3,7,9]) 

cycPrime :: Integral a => [a] -> a -> Bool
cycPrime p n = if digitFilter d 
	       then all (flip (elem) p) $ map list2num $ allRot d 
	       else False
	where d = digSum n

run :: Integral a => a -> Int
run n = (+) 2 $ length $ filter (cycPrime p) [2..n]
	where p = takeWhile (<n) Primes.primes

main = print $ run $ 10^6
