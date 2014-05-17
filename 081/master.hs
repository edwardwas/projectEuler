import Control.Lens
import Data.Maybe

--Type Declerations--
type Matrix = [[Integer]]
type Vector = [Integer]
data Tree = Node Int Integer [Tree] deriving (Show)

--Constant Declerations--
infinity = 9999999999

--Testing Values--
nodes = [121,956,37,331]

--Helper Functions --
blankPaths :: Int -> Matrix
blankPaths l = replicate l $ replicate l infinity :: Matrix

populatePaths :: Vector -> Matrix
populatePaths n = foldr (\(u,v) acc -> set (ix u . ix v) (n !! v) acc) b v
	where v = makeValidPaths l
	      b = blankPaths $ l*l
	      l = floor $ sqrt $ fromIntegral $ length n


getNeigh :: Matrix -> Int -> [(Int,Integer)]
getNeigh p t = filter (\(u,v) -> v /= infinity) $ zip [0..] $ map (getVertex p t) [0..(length p)-1]

getVertex :: Matrix -> Int -> Int -> Integer
getVertex p t f = fromJust $ preview (ix t . ix f) p

makeValidPaths :: Int  -> [(Int,Int)]
makeValidPaths l = lr ++ tb
	where lr = map (\x -> (x,x+1)) $ filter (\x -> mod (x+1) l /= 0) [0..l*l-1]
	      tb = map (\x -> (x,x+l)) [0..l*l - l - 1]

shortPath :: Matrix -> Int -> Int -> Int -> Integer
shortPath p i j 0 = getVertex p i j
shortPath p i j k = min s1 $ s2 + s3
	where s1 = shortPath p i j (k-1)
	      s2 = shortPath p i k (k-1)
	      s3 = shortPath p k j (k-1)

main = print $ shortPath pp 0 l l
	where pp = populatePaths nodes
	      l = (length nodes) - 1
	      nodes = [1..4^2]
