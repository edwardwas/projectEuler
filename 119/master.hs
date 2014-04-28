import Data.List

digits :: Integral a => a -> [a]
digits = map (flip (mod) 10) . takeWhile (>0) . iterate ( flip (div) 10)

digitalSum :: Integral a => a -> a
digitalSum = sum . digits

powerList :: Integral a => a -> Int -> [a]
powerList x n = take n $ iterate (*x) x

validList :: Integral a => a -> Int -> [a]
validList x n = filter (\y -> digitalSum y == x) $ powerList x n

totalList :: Integral a => a -> Int -> [a]
totalList x m = dropWhile (<10) $ sort $ concatMap (\y -> validList y m) [1..x]

run :: Integral a => a -> Int -> a
run x m = (totalList x m) !! 29 

main = print $ run 400 50
