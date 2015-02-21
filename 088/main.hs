import System.Environment
import Data.List
import Data.Array

bigNum :: Integer
bigNum = 10^7

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

factorWaysCal :: Integer -> [[Integer]]
factorWaysCal 2 = [[2]]
factorWaysCal x = [[x]] ++ (concat $ map helper [2.. intSqrt x])
	where helper a = map (a:) $ factorWays $ div x a

factorWays :: Integer -> [[Integer]]
factorWays = (!) arr
	where arr = array (2,bigNum) $ zip [2..bigNum] $ map factorWaysCal [2..bigNum]

sumProdNum :: Integer -> [(Integer,Integer,Integer)]
sumProdNum = (!) (array (2,bigNum) $ zip [2..bigNum] $ map helper [2..bigNum])
	where helper = map (\x -> (sum x, product x, fromIntegral $ length x)) . factorWays

validFactor :: Integer -> [Integer] -> Bool
validFactor k p = (product p) == ( (sum p) + k - (fromIntegral $ length p))

validNumber' :: Integer -> Integer -> Bool
validNumber' k = any (\(s,p,l) -> p == s + k -l) . sumProdNum

validNumber :: Integer -> Integer -> Bool
validNumber k = any (validFactor k) . factorWays

smallestValidNumber :: Integer -> Integer
smallestValidNumber k = head $ filter (validNumber' k) [k..]

main = getArgs >>= print . sum . nub . map smallestValidNumber . enumFromTo 2 . read . head
