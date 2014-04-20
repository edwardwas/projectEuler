import Data.List
import Data.List.Ordered (minus)
import Data.Array

primes :: Int-> [Int]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l
		      intSqrt = floor.sqrt.fromIntegral

p = primes (10^2)

primeTest :: Int-> Bool
primeTest x = elem x p
primesLess :: Int -> [Int]
primesLess x = takeWhile (<=x) p

addToAll :: Ord c => [[c]] -> [[c]] -> [[c]]
addToAll x y = [a ++ b | a <- x, b <- y ]

waysToSum :: Int -> [[Int]]
waysToSum x = (listArray (2,100) $ map f [2..100]) ! x
	where   f 2 = [[2]]
		f 3 = [[3]]
		f n = filterSets $ concatMap helper  $ primesLess (div n 2)
			where filterSets = nub . map sort
			      helper m  
				| primeTest (n-m) =  addToAll ( [m] : waysToSum m) ([n-m] : waysToSum (n-m) )
				| otherwise =  addToAll ( [m] : waysToSum m) (waysToSum (n-m) ) 

primePartition :: Int -> Int
primePartition n
	| primeTest n = 1 + ans
	| otherwise = ans
	where ans = length $ waysToSum n

main = print $ show $ takeWhile (\(u,v) -> v <= 5000) $ zip [2..] $ map primePartition  [2..]
