import Data.Numbers.Primes
import Math.NumberTheory.Moduli

isValid :: Integer -> Integer -> Bool
isValid k = (==) 1 .  powerMod 10 l . (* 9)
	where l = 10^k :: Integer

main = print $ sum $ filter (not . isValid 16) $ takeWhile (<100000) primes


