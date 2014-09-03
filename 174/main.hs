intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

factors :: Integral a => a -> [(a,a)]
factors n = map (\x -> (div n x,x)) $ filter ( (==0) . mod n) [2,4.. intSqrt n]

isValid :: Integral a => (a,a) -> Bool
isValid (x,y) 
	| x == y = False
	| odd x = False
	| otherwise = True

calL :: Integral a => a -> Int
calL = length . filter (isValid) . factors 

calT :: Eq a => [a] -> a -> Int
calT lVal x = length $ filter (==x) $ lVal

main = print $ sum $ map (calT lVal) [1..10]
	where lVal = map calL [4,8..10^6]
