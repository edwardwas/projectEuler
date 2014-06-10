import Data.List
import Data.Ord
import Data.Maybe
import Data.Either
import System.Environment 
------Imports for CSV-------------
import Text.ParserCombinators.Parsec
import Data.CSV

data Edge = Edge Int Int Integer deriving (Eq,Show)
--Edge = State Node, End Node, Weight
type Tree = [Edge]

edgeWeight :: Edge -> Integer
edgeWeight (Edge _ _ w) = w


primms :: Tree -> Tree
primms initTree = helper [startEdge] $ delete startEdge initTree
        where startEdge = minWeight initTree
              helper v o
                | expandingNodes v o == [] = v
                | otherwise = helper (newV:v) $ delete newV o
                where newV = (minWeight $ expandingNodes v o)

nodesInTree :: Tree -> [Int]
nodesInTree = nub . concat . map (\(Edge a b _) -> [a,b])

expandingNodes :: Tree -> Tree ->  [Edge]
expandingNodes t = filter (\(Edge a b _) -> (elem a e) && (not $ elem b e)) 
        where e = nodesInTree t 

minWeight :: [Edge] -> Edge
minWeight = minimumBy (comparing edgeWeight)

treeWeight :: Tree -> Integer
treeWeight = sum . map edgeWeight

removeBack :: Tree -> Tree
removeBack = filter (\(Edge a b _) -> a > b)

primmSaving :: Tree -> Integer
primmSaving t = (treeWeight $ removeBack t) - (treeWeight $ primms t)

list2Edges :: [[Maybe Integer]] -> Tree
list2Edges l = map fromJust $ filter (/= Nothing) $
        map helper [(x-1,y-1) | x <- [1.. length l], y <- [1..length l]] 
        where helper (x,y) = if w == Nothing then Nothing else Just (Edge x y (fromJust w))
                where w = (l !! x) !! y

string2Edges :: [[String]] -> Tree
string2Edges = list2Edges . map (map (\x -> if x == "-" then Nothing else Just (read x :: Integer)) ) 

main = do
        args <- getArgs
        results <- parseFromFile csvFile $ head args
        print $ primmSaving $ string2Edges $ head $ rights $ [results]
