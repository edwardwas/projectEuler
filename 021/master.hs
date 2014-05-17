import qualified Data.Numbers.Primes as Primes
import qualified Data.Vector as V
import Data.List


primeFactors :: Integral a => [a] -> a -> [a]
primeFactors _ 1 = []
primeFactors p n = h : (primeFactors p $ div n h)
	where h = head $ filter (\x -> 0 == mod n x) $ takeWhile (<= n) p

primeExponents :: Integral a => [a] -> a -> [(a,Int)]
primeExponents p = map (\x -> (head x, length x)) . group . primeFactors p

d :: Integral a => [a] -> a -> a
d p n = (div num demon) - n
	where num = product $ map (\(p,a) -> p^(a+1) - 1) exp
              demon = product $ map(\(p,_) -> p - 1) exp
	      exp = primeExponents p n

dVec p maxN = V.map (d p) $ V.enumFromN 1 maxN

amicable v n = n == v V.! ( (v V.! (n-1)) -1)
perfect v n = n == v V.! (n-1)

run p maxN = V.sum $ V.filter (not . perfect d) $ V.filter (amicable d) $  V.enumFromN 2 maxN 
	where d = dVec p $ maxN*10

main = print $ run Primes.primes 10000
