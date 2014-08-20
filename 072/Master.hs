import Data.List
import Data.Ratio

intSqrt :: Integral a => a -> a
intSqrt = floor. sqrt . fromIntegral

brute d = length $ [(x,y) | x <- [1..d], y <- [2..d] , x < y && gcd x y == 1]

primeFactors :: Integral a => a -> [a]
primeFactors x
	| f == [] = [x]
	| otherwise = p : (primeFactors $ div x p)
	where f = filter (\n -> mod x n == 0) [2.. intSqrt x]
	      p = head f

totient :: Integral a => a -> a
totient n = numerator $ (*) (n%1) $ product $ map (\x -> (x-1) % x) $ primeFactors n

run x = sum $ map (totient) [2..x+1]

