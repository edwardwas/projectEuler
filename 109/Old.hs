import Control.Monad
import Data.List
import Data.Function

data Multiplier = Single | Double | Triple
    deriving (Show,Eq,Ord)
type Score = (Multiplier, Int)

calScoreNum (Single,x) = x
calScoreNum (Double,x) = x*2
calScoreNum (Triple,x) = x*3

allPossibleScores :: [Score]
allPossibleScores = zip (repeat Single) [1..20] ++
    zip (repeat Double) [1..20] ++
    zip (repeat Triple) [1..20] ++
    [(Single, 25), (Double,25), (Single, 0)]

totalScore :: [Score] -> Int
totalScore = sum . map calScoreNum

isDouble (Double,_) = True
isDouble _ = False

checkouts :: Int -> [[Score]]
checkouts n = 
    filter (\x -> isDouble (head x) && n == totalScore x) $
    replicateM 3 allPossibleScores

checkoutsEqual :: [Score] -> [Score] -> Bool
checkoutsEqual [] [] = True
checkoutsEqual [] x = False
checkoutsEqual x [] = False
checkoutsEqual x y = (head x == head y) 
    && (sortBy compareScore (tail x) == sortBy compareScore (tail y))

compareScore :: Score -> Score -> Ordering
compareScore a b 
    | calScoreNum a == calScoreNum b = compare (fst a) (fst b)
    | otherwise = compare (calScoreNum a) (calScoreNum b)

howManyWaysToCheckOut :: Int -> Int
howManyWaysToCheckOut = length . nubBy checkoutsEqual . checkouts

main = print $ sum $ map howManyWaysToCheckOut [2..99]

