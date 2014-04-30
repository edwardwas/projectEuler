import Data.List.Ordered hiding (union)
import Data.List

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l
		      intSqrt = floor. sqrt . fromIntegral

pList  = primes (20000000)

primeFact :: Integer -> [Integer]
primeFact n
	| factorList == [] = [n]
	| otherwise = (primeFact f) ++ (primeFact $ div n f)
	where factorList = filter (/= n) $ filter (\x -> mod n x == 0) pList
	      f = head factorList


factorialFactorSum :: Integer -> Integer
factorialFactorSum n = sum $ map (\p -> p* (fF n p) ) $ takeWhile (<=n) pList
	where fF x p = sum $ map (\i ->  div x  (p^i)) [1.. floor( logBase p' x')]
		where p' = fromIntegral p
		      x' = fromIntegral x

run :: Integer -> Integer -> Integer
run n k = helper n - (helper k) - (helper (n-k))
	where helper = factorialFactorSum

main = print $ run 20000000 15000000



