import System.Environment (getArgs)

pascal :: [[Integer]]
pascal = iterate newRow [1]
	where newRow a = 1 : zipWith (+) a (tail a) ++ [1]

divBy7 :: Integer -> Bool
divBy7 = (== 0) . flip mod 7 

run = length . concatMap (filter (not . divBy7)) . flip take pascal

main = getArgs >>= print . run . read . head
