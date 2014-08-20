import Data.List
import Data.Numbers.Primes as Primes

below l = init $ helper l 0
	where helper (x:xs) y
		| y < 1000 = x : (helper xs (y+x))
		| otherwise = []
