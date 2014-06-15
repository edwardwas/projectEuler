import Own
import Data.Numbers.Primes as Primes

repFactors :: Integral a => a -> [a]
repFactors n = filter (\x -> modExp 10 n (9*x) == 1) $  Primes.primes

main = print $ sum $ take 40 $ repFactors $ 10 ^ 9
