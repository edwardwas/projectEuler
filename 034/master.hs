
digits :: Integral a => a -> [a]
digits = map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

factorial :: Integral a => a -> a
factorial n = product [1..n]

run :: Integral a => [a]
run = filter helper [3..]
	where helper n = (sum $ map factorial $ digits n) == n

cumsum :: Num a => [a] -> [a]
cumsum l = helper 0 l
	where helper c [] = []
	      helper c (x:xs) = (c+x):(helper (c+x) xs)
