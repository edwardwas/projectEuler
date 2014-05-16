import Data.List

end = 10**2
badNum = [1,3,7,9,13,27]

possibleNum e = [10,20..e]  
possibleNum' e = [10,20..e]  \\ (foldl1 (++) $ map (\x -> [x,x*2..e]) [30,70,130]) 

toCheck :: Integer -> [Integer]
toCheck n = map (\x -> n*n + x) badNum

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

fullPrimes = all isPrime

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = all (\n -> mod x n /= 0) f 
	where f = 2 : [3,5..intSqrt x]

