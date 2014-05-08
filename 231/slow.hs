import Data.List.Ordered hiding (union)

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l

intSqrt :: Integral a => a -> a
intSqrt = floor. sqrt . fromIntegral

toBase :: (Integral a) => a -> a -> [a]
toBase b = reverse . map (flip (mod) b) . takeWhile (>0) . iterate (flip (div) b) 

numDigitGreater l1 l2 = any (\(u,v) -> u > v) $ padLength l1 l2

padLength a b
	| length a == length b = zip a b
	| length a < length b = padLength (0:a) b
	| length a > length b = padLength a (0:b)


carries :: Integral a => a -> a-> a-> a
carries a b p = helper $ reverse $ map(\(u,v) -> u + v) $ padLength (toBase p a) (toBase p b)
	where helper (x:[]) = div x p
	      helper (x:y:xs) = d + (helper ((y+d):xs))
		      where d = div x p

lucasTest :: Integral a => a -> a -> a-> Bool
lucasTest m n p = numDigitGreater (toBase p n) (toBase p m)

kummer :: Integral a => a -> a-> a-> a
kummer n k p = carries (n-k) k p

run n k = sum $ map (\x -> x * (kummer n k x)) $ filter (lucasTest n k) primeList
	where primeList = primes (10^8)

main = print $ run 20000000 15000000

