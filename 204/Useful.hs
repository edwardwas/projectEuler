module Useful
( primes
, intSqrt
, primeFactors
) where

import Data.List.Ordered (minus)

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l
p = primes $ 10^3

primeFactors :: Integer -> [Integer]
primeFactors 2 = [2]
primeFactors 3 = [3]
primeFactors n 
        | factorList  == [] = [n]
        | otherwise = f : (primeFactors $ (div n f))
        where factorList = filter (\x -> mod n x == 0) p
	      f = head factorList
