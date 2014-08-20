import Data.List

digits2Num :: Integral a => [a] -> a
digits2Num = sum . zipWith (*) (iterate (*10) 1) . reverse

main = print $ digits2Num $ flip (!!) (10^6 - 1) $ sort $ permutations [0..9]
