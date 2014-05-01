import Data.List.Ordered hiding (union)
import Data.List


primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l
		      intSqrt = floor . sqrt . fromIntegral
p = primes $ 10^3

primeFactor :: Integer -> [Integer]
primeFactor n
	| factorList == [] = [n]
	| otherwise = f : (primeFactor $ div n f)
	where factorList = takeWhile (<n) $ filter (\y -> mod n y == 0) p
	      f = head factorList

numDiv :: Integer -> Integer
numDiv 1 = 1
numDiv n = (product $ map helper $ group $ primeFactor n) - n
	where helper x = div (p^(a+1) -1) (p-1)
		where p = head x
		      a = length x

amChain :: Integer  -> Int
amChain n 
	| head h == last h = length h
	| otherwise = 0
	where h = helper n []
		where helper x l
			| x >= 10^6 = [x]
			| any (\y -> x == y) l = x:l
			| otherwise = helper (numDiv x) (x:l)

run m = head $ filter (\(u,v) -> v == maxLength) $ zip [2..m] chainLengths
	where chainLengths = map amChain [2..m]
	      maxLength = maximum $ chainLengths

main = print $ run (10^6)
