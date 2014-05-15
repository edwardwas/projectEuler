import Data.List
import Data.Maybe
type Set = [Integer]

printSet :: Set -> String
printSet = helper . foldr (\acc x -> acc ++ x) [] . getRows
	where helper = (foldr (\acc x -> acc ++ x) "") . (map show)

getOuter :: Set -> [Integer]
getOuter = map head . getRows 

getRows :: Set -> [[Integer]]
getRows (a:b:c:d:e:f:g:h:i:j:[]) = [r1,r2,r3,r4,r5]
	where r1 = [a,b,c]
              r2 = [d,c,e]
	      r3 = [f,e,g]
	      r4 = [h,g,i]
	      r5 = [j,i,b]

isValid :: Set  -> Bool
isValid l = and [allDiff l , head l == (minimum $ getOuter l)]

addsRight :: Integer -> Set -> Bool
addsRight m = all (==m) . map sum . getRows

allDiff:: Eq a => [a] -> Bool
allDiff (x:[]) = True
allDiff (x:xs) = and [not (elem x xs), allDiff xs]

validSets :: [Integer] -> [Set]
validSets = filter (isValid) . permutations 

string2Int :: String -> Integer
string2Int x = read x :: Integer

main = print $ maximum $ filter (\x -> 16 == numDigits x) $ map fromJust $ trim Nothing  toTest
	where k = validSets [1..10]
	      toTest = map (\d -> maxSet $ filter (addsRight d) k) [1..]
	      trim p  = takeWhile (/= p) . dropWhile (==p)

maxSet :: [Set] -> Maybe Integer
maxSet s = if s == [] then Nothing else Just (maximum $ map (string2Int.printSet) s)

numDigits :: Integral a => a -> a
numDigits = (+) 1 . floor . (logBase 10) . fromIntegral
