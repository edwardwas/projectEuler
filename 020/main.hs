main = print $ sum $ map (flip mod 10) $ takeWhile (>0) $ iterate (flip div 10) $ foldl1 (*) [1..100]
