iterDig :: Integral a => a -> [a]
iterDig = takeWhile (>0) . iterate (flip (div) 10)

numDigits :: Integral a => a -> Int
numDigits = length . iterDig

revNum :: Integral a => a -> a
revNum x = helper x $ (numDigits x) - 1
	where helper n a
		| n < 10 = n * (10^a)
		| otherwise = ( (10^a) * (mod n 10) ) + (helper (div n 10) (a-1) )

firstDigit :: Integral a => a -> a
firstDigit = last . iterDig 

isReversible :: Integral a => a -> Bool
isReversible n = all odd $ iterDig  $ n + revNum n

run :: Integral a => a -> Int
run x = (*) 2 $ length $ filter isReversible $ filter (even . firstDigit) $ filter odd [1..x] 
main = print $ run (10^9)
