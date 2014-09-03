intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

factors :: Integral a => a -> [(a,a)]
factors n = map (\x -> (div n x,x)) $ filter ( (==0) . mod n) [2,4.. intSqrt n]

isValid :: Integral a => (a,a) -> Bool
isValid (x,y) 
	| x == y = False
	| odd x = False
	| otherwise = True

calL :: Integral a => a -> Int
calL = length . filter (isValid) . factors 

run :: Integral a => a -> Int
run = sum . map calL . enumFromThenTo 4 8 

main :: IO ()
main = print $ run $ 10^6
