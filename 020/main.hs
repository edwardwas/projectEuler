fact :: Integral a => a -> a
fact n = foldl1 (*) [2..n]

digSum :: Integral a => a -> a
digSum = sum . map (flip mod 10) . takeWhile (>0) . iterate (flip div 10)

main :: IO ()
main = print $ digSum $ fact $ 100
