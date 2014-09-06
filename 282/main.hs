
litAck :: Integral a => a -> a -> a
litAck 0 n = n + 1
litAck m 0 = litAck (m-1) 1
litAck m n = litAck (m-1) $ litAck m $ n - 1

kArrow :: Int -> Int -> Int -> Int
kArrow 0 a b = a*b
kArrow 1 a b = a^b
kArrow n a b = foldr1 (kArrow (n-1)) $ replicate b a

mean x = (fromIntegral $ sum x) / (fromIntegral $ length x)

fA m n = (kArrow (m-2) 2 (n+3)) - 3
