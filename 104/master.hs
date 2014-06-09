import Data.List


fib = scanl (+) 0 (1:fib)

int2dig :: Integral a => a -> [a]
int2dig = map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

firstDigits :: Integral a => Int -> a -> [a]
firstDigits n = take n . int2dig

lastDigits :: Integral a => Int -> a -> [a]
lastDigits n = take n . reverse . int2dig

isPanDigital :: Integral a => [a] -> Bool
isPanDigital x = (sort x) == [1..9]

main = print $ snd $ head $ 
        filter (isPanDigital . firstDigits 9 . fst) $ 
        filter (isPanDigital . lastDigits 9 . fst) $ zip fib [0..]
