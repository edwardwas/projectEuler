factorize :: Integral a => a -> [a]
factorize n
        | null f = [n]
        | otherwise = h : (filter $ div n h)
        where h = head f
              f = filter ((== 0) . mod n) [2..intSqrt n]
