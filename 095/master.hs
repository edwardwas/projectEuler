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

run m = head $ filter (\(u,v) -> v == maxLength) $ zip [2..m] chainLengths
	where chainLengths = map amChain [2..m]
	      maxLength = maximum $ chainLengths

main = print $ run (10^6)


amChain n = loopCheck [] $ iterate (numDiv) n
	where	loopCheck [] (x:xs) = loopCheck [x] xs
		loopCheck k (x:xs)
			| x == 1 = 0
			| x >= 10^6 = 0
			| last k == x = (length k) 
			| any (\y -> x ==y) $ init k = 0
			| otherwise = loopCheck (x:k) xs
