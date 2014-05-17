
digSum :: Integral a => a -> [a]
digSum = map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

test n = n == (sum $ map(^5) $ digSum n)

cumSum l = tail $ helper 0 l
	where helper c [] = [c]
	      helper c (x:xs) = c : (helper (c+x) xs)
