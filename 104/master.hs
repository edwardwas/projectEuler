
fib :: [Integer]
fib = [1,1] ++ (helper [1,1])
	where helper l = x : (helper (x:l))
		where x = (head l) + (head $ tail l)

lastDigits :: Integral a => Int -> a -> [a]
lastDigits n = map (flip (mod) 10) . take n . iterate (flip (div) 10) 

panDigital :: Integral a => [a] -> Bool
panDigital l = and $ map (flip (elem) l) [1..9]

numDigits :: Integer -> Integer
numDigits = (+) 1 . floor . logBase 10 . fromIntegral 

