
intSqrt = floor . sqrt . fromIntegral

squares x = [n*n | n <- [1 .. intSqrt x] ]

testN = 10000

f n = (*) 4 $ length $ filter (\x -> elem x s) $ map (\y -> ( div (n*n) 4) - y*y) [0.. div n 2]
	where s = squares $ div (n*n) 4
