import Data.List
type Set = [Integer]

testCase = [4,3,2,6,1,5] :: Set
targetTotal = 9

validNum = [1..6]
start = replicate 6 1

printSet :: Set -> String
printSet l = helper $ foldr (\acc x -> acc ++ x) [] $ getRows l
	where helper = (foldr (\acc x -> acc ++ x) "") . (map show)

getOuter :: Set -> [Integer]
getOuter = map head . getRows 

getRows :: Set -> [[Integer]]
getRows (a:b:c:d:e:f:[]) = [r1,r2,r3]
	where r1 = [a,b,c]
	      r2 = [d,c,e]
              r3 = [f,e,b]

isValid :: Set  -> Bool
isValid l = and [allDiff l , head l == (minimum $ getOuter l)]

addsRight :: Integer -> Set -> Bool
addsRight m l = all (==m) $ map sum $ getRows l

allDiff:: Eq a => [a] -> Bool
allDiff (x:[]) = True
allDiff (x:xs) = and [not (elem x xs), allDiff xs]

addCombElem :: [a] -> [[a]] -> [[a]]
addCombElem vn l = concatMap (helper vn) l
	where helper vn x = [y:x | y <- vn]

allComb :: [Integer] -> Int -> [Set]
allComb vn l = last $ take (l+1) $ iterate (addCombElem vn) [[]]

validSets :: [Integer] -> [Set]
validSets vn  = filter (isValid) $ allComb vn 6

string2Int :: String -> Integer
string2Int x = read x :: Integer

main = print $ take 100 $ zip [1..] $ map maxSet $ map (\d -> filter (addsRight d) k ) [1..]
	where k = validSets [1..10]

maxSet :: [Set] -> Maybe Integer
maxSet s = if s == [] then Nothing else Just (maximum $ map (string2Int.printSet) s)
