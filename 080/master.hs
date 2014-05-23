createY :: Integral a => a -> a -> a
createY p x = x * (20*p + x)

findX :: Integral a => a -> a -> a
findX p c = helper 0 p c
	where helper x p c
		| (createY p x) > c = x -1
		| otherwise = helper (x+1) p c

list2int :: Integral a => a -> [a]
list2int = reverse . map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

list4cal :: Integral a => [a] -> [a]
list4cal l = newL ++ (repeat 0)
	where newL = if even $ length l then l else 0:l

rootDigits :: Integral a => a -> [a]
rootDigits n = helper (list4cal $ list2int n)  0 0
	where helper (l1:l2:ls) p oldR
		| r == 0 = []
		| otherwise = x : (helper ls newP r)
		where r = c - y
		      c = oldR*100 + l1*10 + l2
		      y = createY p x
		      x = findX p c
		      newP = 10*p + x

main = print $ sum $ map (sum . take 100 . rootDigits) [1..100]
