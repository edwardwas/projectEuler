
increment :: (Ord a, Num a) => [a] -> [a]
increment l = helper l []
	where helper (f:s:hs) t
		| f + 1 > s = helper (s:hs) (t ++ [0])
		| otherwise = t ++ (f+1 : s :hs)
	      helper (f:[]) t = t ++ [f+1]

numDivisors :: Num a => [a] -> a
numDivisors = product . map (+1) 

primes = [2,3,5,7,11,13,17,19,23]

calNum :: (Num a, Integral b) => [a] -> [b] -> a
calNum p l = product $ map (\(u,v) -> v^u) $ zip (reverse l) $ take (length l) p

