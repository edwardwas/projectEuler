
fibs :: Integral a => [a]
fibs = tail $ map fst $ iterate(\(a,b) -> (a+b,a)) (0,1)

numDigits :: Integral a => a -> a
numDigits = (+) 1 . floor . flip (/) (log 10) . log . fromIntegral
