import Data.List

data Suit = Heart | Diamond | Spade | Club deriving (Show,Eq)
type Rank = Int
data Card = Card Suit Rank deriving (Show,Eq)
type Hand = [Card]

suit :: Card -> Suit
suit (Card s _) = s

rank :: Card -> Rank
rank (Card _ r) = r

consec :: (Num a, Eq a) => [a] -> Bool
consec (x:[]) = True
consec (x:y:xs) = and [x+1 == y, consec (y:xs)]

rankings :: Hand -> [Int]
rankings = sort . map length . group . sort . map rank 


sameSuit :: Hand -> Maybe Suit
sameSuit h
	| all (==s) sh = Just s
	| otherwise = Nothing
	where s = head sh
	      sh = map suit h

royalFlush :: Hand -> Bool
royalFlush h
	| sameSuit h == Nothing = False
	| otherwise = [10,11,12,13,14] == (sort $ map rank h)

straightFlush :: Hand -> Bool
straightFlush h
	| sameSuit h == Nothing = False
	| otherwise = consec $ sort $ map rank h

fourKind :: Hand -> Bool
fourKind h = [1,4] == rankings h

fullHouse :: Hand -> Bool
fullHouse h = [2,3] == rankings h

flush :: Hand -> Bool
flush h = sameSuit h /= Nothing

straight :: Hand -> Bool
straight h = consec $ sort $ map rank h

threeKind :: Hand -> Bool
threeKind h = 3 == (maximum $ rankings h)

twoPair :: Hand -> Bool
twoPair h = [2,2,1] == rankings h

onePair :: Hand -> Bool
onePair h = [2,1,1,1] == rankings h

highCard :: Hand -> Bool
highCard _ = True

compareRanks :: [Int] -> [Int] -> Bool
compareRanks [] [] = True
compareRanks (a:as) (b:bs)
	| a > b = True
	| a == b = compareRanks as bs
	| otherwise = False

handRanks = [royalFlush, straightFlush,fourKind,fullHouse,flush,straight,threeKind,twoPair,onePair,highCard]

compareHands :: [Hand -> Bool] -> Hand -> Hand -> Bool
compareHands (f:fs) h1 h2
	| and [f h1, not (f h2)] = True
	| and [not (f h1), f h2] = False
	| and [f h1, f h2] = compareRanks (sort $ map rank h1) (sort $ map rank h2)
	| otherwise = compareHands fs h1 h2

