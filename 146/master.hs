import qualified Data.Numbers.Primes as Primes
import Data.List

test p n = (take 6 $ dropWhile (<n*n) p ) ==
	map (+(n*n)) [1,3,7,9,13,27]

vals n = [10,20..n]
	where remItems a b = b \\ a
