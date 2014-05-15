import Data.List

list2int :: Integral a => [a] -> a
list2int = sum . zipWith (*) (iterate (*10) 1) . reverse

run :: Integral a => a -> Int -> a
run n = list2int . (!!) (sort $ permutations [0..n])

main = print $ run 9 $ 10^6 -1
