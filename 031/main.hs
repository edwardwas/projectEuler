
coins :: Integral a => [a]
coins = [1,2,5,10,20,50,100,200]

addToEvery :: a -> [[a]] -> [[a]]
addToEvery c = map ((:) c)

ways :: Integral a => a -> [a] -> [[a]]
ways 0 _ = [[]]
ways n coins = concat $ map (\c -> addToEvery c $ ways (n-c) $ filter (<= c) coins) $ filter (<= n) coins 

main = print $ length $ ways 200 coins
