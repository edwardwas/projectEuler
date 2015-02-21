import Control.Applicative
import Data.List

---Brute Force section for checking

type Block = (Bool, Int)
type Row = [Block]

findBlocks :: Int -> [Row]
findBlocks 0 = []
findBlocks 1 = [[(False,1)]]
findBlocks 2 = [[(False,2)]]
findBlocks 3 = [[(True,3)],[(False,3)]]
findBlocks n = concatMap (\b -> combsFromTuple $ (b,n-b)) [3..n-1]

combineAll :: [[a]] -> [[a]] -> [[a]]
combineAll a b = (++) <$> a <*> b

combsFromTuple :: (Int,Int) -> [Row]
combsFromTuple (a,b) = combineAll (findBlocks a) (findBlocks b)

reduceTerms :: Row -> Row
reduceTerms ((a1,n1):(a2,n2):xs)
	| a1 == a2 = reduceTerms $ (a1,n1+n2):xs
	| otherwise = (a1,n1) : (reduceTerms $ (a2,n2):xs)
reduceTerms (x:[]) = [x]
reduceTerms [] = []
