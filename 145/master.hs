iterDig :: Integral a => a -> [a]
iterDig = takeWhile (>0) . iterate (flip (div) 10)

numDigits :: Integral a => a -> Int
numDigits = length . iterDig

revNum :: Integral a => a -> a

revNum n = sum $ map (\(u,v) -> u*v) $ zip a b
	where a = reverse $ map (\x -> mod x 10) $ iterDig n
	      b = take (length a) $ iterate (*10) 1

firstDigit :: Integral a => a -> a
firstDigit = last . iterDig 

isReversible :: Integral a => a -> Bool
isReversible n = all odd $ iterDig  $ n + revNum n

run :: Integral a => a -> Int
run x = (*) 2 $ length $ filter isReversible $ filter (even . firstDigit) [1,3..x]
main = print $ run (10^8)
