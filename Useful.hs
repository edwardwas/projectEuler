module Useful(
,intSqrt
,primes
,primeFactors
) where

import Data.List
import Data.List.Ordered (minus)

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l

primeFactors :: Integral a => a -> [a]
primeFactors 2 = [2]
primeFactors 3 = [3]
primeFactors n
        | f == [] = [n]
        | otherwise = primeFactors (head f) ++ primeFactors ( div n (head f) )
        where f = filter (\x -> mod n x == 0) [2 .. intSqrt n]
