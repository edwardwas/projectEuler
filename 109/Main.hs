import Control.Monad

data Score = Miss | Single Int | Double Int | Triple Int deriving (Show,Eq,Ord)

allPossibleScores :: [Score]
allPossibleScores = Miss :
    map Single [1..20] ++
    map Double [1..20] ++
    map Triple [1..20] ++
    [Single 25, Double 25]

scoreValue :: Score -> Int
scoreValue (Single x) = x
scoreValue (Double x) = 2*x
scoreValue (Triple x) = 3*x
scoreValue Miss = 0

isDouble :: Score -> Bool
isDouble (Double _) = True
isDouble _ = False

makeCheckouts :: Int -> [[Score]]
makeCheckouts n = do
    f <- filter isDouble allPossibleScores
    s <- allPossibleScores
    d <- filter (>= s) allPossibleScores
    if sum (map scoreValue [f,s,d]) < n then return [f,s,d] else mzero

main = print $ length $ makeCheckouts 100
    
