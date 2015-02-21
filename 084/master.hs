import Control.Applicative
import Data.List
import Data.Maybe
import Numeric.Container
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms

data Square = GO | A1 | CC1 | A2 | T1 | R1 | B1 | CH1 | B2 | B3 | JAIL
	| C1 | U1 | C2 | C3 | R2 | D1 | CC2 | D2 | D3 | FP
	| E1 | CH2 | E2 | E3 | R3 | F1 | F2 | U2 | F3 | G2J
	| G1 | G2 | CC3 | G3 | R4 | CH3 | H1 | T2 | H2
	deriving (Eq,Show,Enum,Bounded,Ord)

type Prob = (Square,Double)

diceMoves :: (Num b, Enum b) => b -> [b]
diceMoves n = (+) <$> [1..n] <*> [1..n]

move :: Square -> Int -> Square
move start n = toEnum $ mod x (fromEnum (maxBound :: Square))
	where x = n + fromEnum start

possibleMoves :: Int -> Square -> [Prob]
possibleMoves n start = zip (map (move start) $ diceMoves n) $ repeat 1

count :: Eq a => [a] -> a -> Int
count l x = length $ filter (==x) l

normalise :: [Prob] -> [Prob]
normalise p = map (\(a,b) -> (a,b/m)) p
	where m = sum $ map snd p

collect :: [Prob] -> [Prob]
collect p = map (\l -> (fst $ head l, sum $ map snd l)) $ group $ sort p

makeListForMatrix :: Int -> Square -> [Double]
makeListForMatrix n s = map (fromMaybe 0 . flip lookup a) $ enumFrom GO
	where a = normalise $ collect $ possibleMoves n s

makeMatrix :: Int -> Matrix Double
makeMatrix n = fromLists $ map (makeListForMatrix n) $ enumFrom GO
