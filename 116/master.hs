
fitings rl tl
        | tl > rl = 0
        | tl == rl = 1
        | otherwise = sum $ map (\x -> 1 + (fitings (rl - tl - x) tl )) [0..(rl-tl)]

main = print $ sum $ map (fitings 50) [2,3,5]
