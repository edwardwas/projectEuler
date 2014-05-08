import Data.List hiding (reverse)
import Data.List.Ordered hiding (union,nub)

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l

numDigits n = 1 + (floor $ logBase 10 $ fromIntegral n)

toDigitList n
	| n < 10 = [n]
	| otherwise = mod n 10 : (toDigitList $ div n 10)
toNum l = sum $ nub [x*y | x <- tens, y <-l]
	where tens = init $ scanl (\acc x -> 10*acc) 1 l

applyMask n m = map toNum $ map (\t-> map (\z -> helper t z) $ zip (reverse m) (toDigitList n)) [0..9]
	where helper t (x,y) 
		| x == True  = t
		| otherwise = y

count n l = length $ filter (==n) l

allMasks n = filter helper  $ sequence $ replicate n [True,False]
	where helper x = and [count True x == 3,last x /= True]

p = primes (10^7)
toFilter = filter (\x -> numDigits x == 6) p


