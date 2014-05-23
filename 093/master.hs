import Data.List


comb 0 _ = [[]]
comb _ [] = []

comb n (x:xs)
	| n == 1 = map (:[]) (x:xs)
	| otherwise = [x:c | c <- comb (n-1) xs] ++ comb n xs

arFuns = [(+),(-),(*),(/)]

