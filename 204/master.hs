import Useful
import Data.List

merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x

merge (x:xs) (y:ys) 
	| x < y = x : merge xs (y:ys)
	| x > y = y : merge (x:xs) ys
	| x == y = x : merge xs ys

genHam [] = []
genHam (x:xs) = out
	where out = merge (1 : map (*x) out) (genHam xs)

run n p = length $ takeWhile (<n) $ genHam $ primes p

main = print $ 1 + (run (10^9) 100)
