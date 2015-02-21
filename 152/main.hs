import Data.List
import Data.Ratio
import Data.Monoid
import Data.Tree


testTree = Node 2 [Node 4 [], Node 3 []]

options = [2..5]

makeTree :: (Ord a, Num a) => [a] -> a -> a -> Tree a
makeTree opts a x = Node (a+x) $ map (makeTree opts (a+x)) $ dropWhile (>=x) opts

