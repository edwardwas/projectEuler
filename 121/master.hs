import Data.Ratio
import Data.List
data Disc = Red | Blue deriving (Show,Eq)

probOfBlue n = 1 % (n+1)
probOfRed n = 1 - (probOfBlue n)

bag n = Blue : (replicate n Red)

addChoices :: [[Disc]] -> Int -> [[Disc]]
addChoices b n = concatMap (\oldBag -> map (\c -> c:oldBag) newBag) b
	where newBag = bag n


allChoices x = helper [[Red],[Blue]] 2
	where helper b n
		| n == x = addChoices b x
	        | otherwise = helper (addChoices b n) (n+1)

replaceWithProb n d
	| d == Red = probOfRed n
	| otherwise = probOfBlue n  

choices2Prob d = product $ map (\(u,v) -> replaceWithProb u v) $ zip [1..] $ reverse d

count f l = length . filter f
