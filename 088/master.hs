import Data.List
import Data.Array

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

maxK = 12000

isFactor :: Integral a => a -> a -> Bool
isFactor x y = mod x y == 0

factors :: Integral a => a -> [[a]]
factors n
	| f == [] = [[n]]
	| [2..intSqrt d] == []  = [[h,d]]
	| otherwise = [ [h,d] ] ++ (map (h:) $ factors d)
	where f = filter (isFactor n) $ [2 .. intSqrt n]
	      h = head f
	      d = div n h
	
allFact :: Integral a => a -> [[a]]
allFact x = concatMap (\r -> map (r:) $ [[div x r]] ++ factors (div x r)) 
	$ filter (isFactor x) [2.. intSqrt x]

getK f = (product f) - (sum f) + (length f)

kValues x = arr ! x
	where arr = array (2,maxK) [(y, nub $ map getK $ allFact y) | y <- [2..maxK]]

minProdSum kV k = minimum $ map fst $ filter (elem k . snd) $ zip [k..2*k] $ map kValues [k..2*k]

mPS k = minimum $ map fst $ filter (elem k . snd) $ zip [k..k*2] $ map (map getK) $ map allFact [k..k*2]

run k = sum $ nub $ map mPS [2..k]

main = print $ run $ maxK+1
