import Data.List hiding (reverse)
import Data.List.Ordered hiding (union)

numDigits :: Integral a => a -> a
numDigits n  = (floor $ logBase 10 $ fromIntegral  n ) + 1

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l

toDigitList :: Integral a => a -> [a]
toDigitList n = (helper n)
	where helper n
		| n < 10 = [n]
		| otherwise = mod n 10 : (toDigitList $ div n 10)


elementwise :: (t1 -> t2 -> o) -> [t1] -> [t2] -> [o]
elementwise f (a:as) (b:bs) = (f a b) : elementwise (f) as bs
elementwise _ [] [] = []
elementwise _ _ _ = error "Length mismatch"

toNum :: Integral a => [a] -> a
toNum l = sum $ elementwise (*) l tens
	where tens = init $ scanl (\acc x -> 10*acc) 1 l

applyMask :: [a] -> [Bool] -> a -> [a]
applyMask l m n = map (\(x,y) -> change x y) $ zip l m
	where change x y
		| y == True = n
		| otherwise = x

allNums n m = filter lenCheck $ map (\x -> toNum $ applyMask l (reverse m) x) [0..9]
	where l = toDigitList n
	      lenCheck x = (numDigits x) == (numDigits n)

allMasks n = filter helper $ sequence $ replicate n [True,False]
	where helper x = not $allSame x

allSame [] = False
allSame (x:[]) = False
allSame (x:xs) = or [x == head(xs), allSame xs]

count x l = length $ filter (==x) l

allMaskNums n = map (\m -> allNums n m) $ allMasks $ numDigits n

countPrimes n = map (\x -> length $ filter isPrime x) $ allMaskNums n

p = primes (10^7)
isPrime n = elem n p

sol n = find (\x -> 6 == maximum (countPrimes x)) [10..]
