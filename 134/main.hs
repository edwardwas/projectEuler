import Data.Numbers.Primes as Primes

allMultiples :: Num a => a -> [a]
allMultiples n = iterate(+n) n

lastNDigits :: Integral a => Int -> a -> [a]
lastNDigits n = map (flip mod 10) . take n . iterate (flip div 10)

allDigits :: Integral a => a -> [a]
allDigits = map (flip mod 10) . takeWhile (>0) . iterate (flip div 10)

findS :: Integral a => a -> a -> a
findS p1 = head . filter ((==) d . lastNDigits l) . allMultiples 
	where d = allDigits p1
	      l = length d

toSearch :: Integral a => [(a,a)]
toSearch = takeWhile ((>) 1000000 . fst) $ zip d $ tail d
	where d = drop 2 $ Primes.primes

main = print $ sum $ map (uncurry findS) toSearch
