import Data.List (delete)

firstDigs :: (Integral a, Integral b, Integral c) => a -> b -> c
--Works using the golden ratio approach
firstDigs df nf = floor $ 10 ** (digs - (fromIntegral $ floor digs) +d - 1)
	where digs = n * 0.20898764024997873 - 0.3494850021680094
	      n = fromIntegral nf
	      d = fromIntegral df

fibs :: Integral a => [a]
fibs = helper 1 1
	where helper a b = b : helper (flip mod (10^9) $ a+b) a

digs :: Integral a => a -> [a]
digs = map (flip mod 10) . takeWhile (>0) . iterate (flip div 10)

isPandigital :: Integral a => [a] -> Bool
isPandigital = helper [1..9]
	where 	helper [] [] = True
	  	helper (x:xs) l
			| elem x l = helper xs $ delete x l
			| otherwise = False

findFirst :: (a -> Bool) -> [a] -> a
findFirst f = head . filter f

main :: IO ()
main = print $ fst $ findFirst (validDigs . (firstDigs 9) . fst) 
	$ filter (validDigs . snd) $ zip [1..] fibs
		where validDigs = isPandigital . digs

