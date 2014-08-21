import Data.Array (array,(!),Ix)
import Data.Numbers.Primes (primes)
import System.Environment (getArgs)

isSquareFree :: (Integral a, Ix a)  => a -> Bool
isSquareFree x = dp ! x
	where 	dp = array (1,10^6) [(i, helper i []) | i <- [1..10^6]]
		helper 1 _ = True
	      	helper x l
			| elem f l = False
			| otherwise = helper (div x f) $ f:l
			where f = nextPrimeFactor x

nextPrimeFactor :: Integral a => a -> a
nextPrimeFactor x = head $ filter ((==0) . mod x) primes

factors :: Integral a => a -> [a]
factors n = filter ((== 0) . mod n ) [2..n]

rad :: (Ix a,Integral a) => a -> a
rad 1 = 1
rad x = dp ! x
	where f x = head $ filter (isSquareFree) $ reverse $ factors x
	      dp = array (2,10^6) [(i,f i) | i <- [2..10^6]]

genAllC :: (Ix a, Integral a) => a -> a -> [(a,a)]
genAllC l a = zip (repeat a) $ filter (not . isSquareFree) $ filter ((==1) . gcd a) $ [2*a+1..l-1]

hit :: (Integral a, Ix a)  => (a,a) -> Bool
hit (a,c) = c > (product $ map rad [a,b,c])
	where b = c - a

getAll :: (Integral a, Ix a)  => a -> [(a,a)]
getAll l = filter (hit) $ concatMap (genAllC l) $ [1..(div (l-1) 2) - 1]

main = getArgs >>= return . sum . map snd . getAll . read . head >>= print
