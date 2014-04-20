import Data.List
import Data.List.Ordered (minus)
import Data.Array

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l
		      intSqrt = floor.sqrt.fromIntegral

p = primes (10^6)

primeTest :: Int -> Bool
primeTest x = elem x p
primesLess x = takeWhile (<=x) p


addToAll :: [[a]] -> [[a]] -> [[a]]
addToAll x y = [a ++ b | a <- x, b <- y]

setArr = listArray (2,100) $ map f [2..100]
accArr x = setArr ! x

f 2 = [[2]]
f 3 = [[3]]
f n = filterSets $ concatMap helper  $ primesLess (div n 2)
	where helper m
		| and [primeTest m, primeTest(n-m)] = addToAll ( [m] : accArr m) ([n-m] : accArr (n-m) )
		| primeTest m = addToAll ( [m] : accArr m) (accArr (n-m) ) 
		| primeTest (n-m) = addToAll ( accArr m) ([n-m] : accArr (n-m) ) 
		| otherwise = addToAll (accArr m) (accArr (n-m) )

priePart' n
	| primeTest n = 1 + (length $ accArr n)
	| otherwise = length $ accArr n



filterSets :: Ord a => [[a]] -> [[a]]
filterSets = nub . map sort 

main = print $ show $ takeWhile (\(u,v) -> v <= 5000) $ zip [2..] $ map priePart' [2..]
