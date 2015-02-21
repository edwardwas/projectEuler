import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid

faceOptions :: [[Integer]]
faceOptions = [[0],[1],[2],[3],[4],[5],[6,9],[7],[8]]

concNumbers :: Integral a => a -> a -> a
concNumbers a b = b + 10*a

allSquares :: [Integer]
allSquares = zipWith (*) [1..9] [1..9]

possibleNumbers :: Integral a => [a] -> [a] -> [a]
possibleNumbers a b = (concNumbers <$> a <*> b) ++ (concNumbers <$> b <*> a)

testTwo :: Integral a => [a] -> [a] -> [a] -> Bool
testTwo sqrs a b = all(\x -> elem x l) sqrs
	where l = possibleNumbers a b

increasing :: Ord a => [a] -> Bool
increasing x = and $ zipWith (<) x $ tail x

addRow :: Ord a => [a] -> [[a]] -> [[a]]
addRow opts x = (:) <$> opts <*> x

createCombs :: Ord a => Int -> [a] -> [[a]]
createCombs n opts = last $ take (n+1) $ iterate (addRow opts) [mempty]

allPossibleCubes :: [[Integer]]
allPossibleCubes = nub $ map concat $ createCombs 6 faceOptions

run = length $ filter (uncurry $ testTwo allSquares) $ (,) <$> a <*> a
	where a = allPossibleCubes

main = print run
