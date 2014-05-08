reduce :: Integral a => [a] -> a -> a
reduce [] n = n
reduce (x:xs) n | mod n x == 0 = reduce (x:xs) (div n x)
		| otherwise = reduce xs n

terminating :: Integral a => a -> a -> Bool
terminating p q = 1 == reduce [2,5]  (div q $ gcd p q)

d x = sum [if terminating n (round $ fromIntegral n /e) then -n else n | n <- [5..x]]
	where e = exp(1)

main = print $ d 10000
