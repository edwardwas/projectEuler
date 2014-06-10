import Data.List

data Edge = Edge Int Int Integer deriving (Show,Eq)
--Edge = Start End Weight
data Node = Node Bool Integer deriving (Show)
--Node = Visited tentative number

bigNum :: Integer
bigNum = 99999999999


isFrom :: Int -> Edge -> Bool
isFrom i (Edge a _ _) = i == a

isNodeVisited :: Node -> Bool
isNodeVisited (Node v _) = v

vertexsFrom :: Int -> [Edge] -> [Edge]
vertexsFrom node = filter (isFrom node)

visited :: [Node] -> [Node]
visited = filter (\(Node v _) -> v)

unvisited :: [Node] -> [Node]
unvisited = filter (\(Node v _) -> not v)

nodesInTree :: [Edge] -> [Int]
nodesInTree = nub . concat . map (\(Edge a b _) -> [a,b])

startList :: [Edge] -> [Node]
startList e = (Node True 0):(replicate (length $ nodesInTree e) (Node False bigNum))
