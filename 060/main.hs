import Data.Numbers.Primes 
import Control.Applicative
import Data.Array.IArray
import Data.List

maxNum :: Integral a => a
maxNum = 3*(10^3)

numToDigits :: Integral a => a -> [a]
numToDigits = map (flip mod 10) . takeWhile (/= 0) . iterate (flip div 10)

digitsToNum :: Integral a => [a] -> a
digitsToNum = sum . zipWith (*) (iterate (*10) 1)

concNumbers :: Integral a => a -> a -> a
concNumbers x y = digitsToNum $ (numToDigits x) ++ (numToDigits y)

primesBelow :: Integral a => a -> [a]
primesBelow x = takeWhile (<= x) primes

testTwo :: Integral a => a -> a -> Bool
testTwo x = isPrime . concNumbers x

testSet :: (Ix a, Integral a) => [a] -> Bool
testSet xs = and $ [testTwo a b | a <- xs, b <- xs, a /= b]

possiblePairs :: Integral a => a -> [a]
possiblePairs x = filter (testTwo x) $ primesBelow x

possibleFromSet :: (Ix a, Integral a) => [a] -> [a]
possibleFromSet (x:[]) = possiblePairs x
possibleFromSet (x:xs) = foldl (intersect) (possiblePairs x) $ map possiblePairs xs

twoArr :: (Ix a, Integral a) => Array (a,a) Bool
twoArr = array ((1,1),(maxNum,maxNum)) [ ((a,b),testTwo a b) | a <- [1..maxNum], b <- [1..maxNum]]

getTwo :: (Ix a, Integral a) => a -> a -> Bool
getTwo = curry $ (!) twoArr


main = do
	print $ testSet [3,7,109,673]
	print "boop"
	print $ testSet [3,7,109,673]
