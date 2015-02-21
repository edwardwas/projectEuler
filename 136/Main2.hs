import System.Environment

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

factorPairs :: Integral a => a -> [(a,a)]
factorPairs n = map (\a -> (div n a,a)) $ filter ((== 0). mod n) $ [1.. n]

validPair :: Integral a => (a,a) -> Bool
validPair (u,v) = 3*u > v && mod (u+v) 4 == 0 && mod (3*u - v) 4 == 0

run :: Integer -> Int
run = length . filter (== 1) . map (length . filter (validPair) . factorPairs) .  enumFromTo 1

main = getArgs >>= print . run . read . head
