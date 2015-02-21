import Data.Numbers.Primes hiding (isPrime)

type Mask = [Integer]

digs2Num :: Integral a => [a] -> a
digs2Num = sum . zipWith (*) (iterate (*10) 1) . reverse

num2Digs :: Integral a => a -> [a]
num2Digs = reverse . map (flip mod 10) . takeWhile (>0) . iterate (flip div 10)

invert :: (Integral a, Eq a) => a -> a
invert 0 = 1
invert 1 = 0
invert _ = error "Invert only works on 1 or 0"

applyMask :: [Integer] -> Mask -> Integer -> [Integer]
applyMask start mask n = zipWith (+) (map (*n) mask) $ zipWith (*) start $ map invert mask

genAllMasks :: Int -> [Mask]
genAllMasks n = tail $ init $ sequence $ replicate n [0,1]

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime p = null $ filter ( (==0) . mod p) [2..intSqrt p]
	where intSqrt = round . sqrt . fromIntegral 

howManyPrimes :: Integer -> Int
howManyPrimes p = maximum $ map (\m -> length $ filter (isPrime) $ map (digs2Num . applyMask dp m) 
		[0..9]) $ genAllMasks $ length dp
	where dp = num2Digs p

run n = filter ( (== n) . snd) $ zip k $ map howManyPrimes k
	where k = dropWhile (<=9) primes
