import Data.Array
import Data.List.Ordered (minus)
import qualified Data.MemoCombinators as Memo

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l

intSqrt :: Integral a => a -> a
intSqrt = floor.sqrt.fromIntegral

limit = 40000000 :: Integer
p = primes(limit)

totient :: Integral a => a -> a
totient n
	| f == [] = n-1
	| otherwise = (totient d) * (totient $ div n d)
	where f = filter (\x -> mod n x == 0) [2 .. intSqrt n]
	      d = head f

chain :: Integer -> Integer
chain n = Memo.integral chain' n
	where chain' 1 = 1
              chain' x = 1 + (chain $ totient x)

run n = filter (\x -> chain (x-1)  == 24) $ takeWhile (< n) p

main = print $ sum $ run 40000000
