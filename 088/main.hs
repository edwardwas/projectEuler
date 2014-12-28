import System.Environment
import Data.List

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

factorWays :: Integral a => a -> [[a]]
factorWays 2 = [[2]]
factorWays x = [[x]] ++ (concat $ map helper [2.. intSqrt x])
	where helper a = map (a:) $ factorWays $ div x a

validFactor :: Integral a => a -> [a] -> Bool
validFactor k p = (product p) == ( (sum p) + k - (fromIntegral $ length p))

validNumber :: Integral a => a -> a -> Bool
validNumber k = any (validFactor k) . factorWays

smallestValidNumber :: Integral a => a -> a
smallestValidNumber k = head $ filter (validNumber k) [2..]

main = getArgs >>= print . sum . nub . map smallestValidNumber . enumFromTo 2 . read . head
