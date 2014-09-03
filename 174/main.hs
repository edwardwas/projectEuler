

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

factorPairs :: Integral a => a -> [(a,a)]
factorPairs n = map (\x -> (x,div n x)) $ filter ((== 0) . mod n) [2..intSqrt n]

bothEvenOrOdd :: Integral a => (a,a) -> Bool
bothEvenOrOdd (x,y)
	| x == y = False
	| odd x = odd y
	| otherwise = even y

l :: Integral a => a -> Int
l = length . filter bothEvenOrOdd . factorPairs

calN :: [Int] -> Int -> Int
calN  lValues n = length $ filter (== n) lValues

main = print $ calN lVals 15
	where lVals = map l [1..10^6]
