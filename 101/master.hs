import Numeric.LinearAlgebra
import Data.List
import GHC.Float

integral2Double :: Integral a => a -> Double
integral2Double = float2Double . fromIntegral

genInputMatrix :: (Field t, Element t, Num t) => [t] -> Matrix t
genInputMatrix s = fromLists $ map (\x -> take l $ iterate (*x) 1) s
	where l = length s

makeVector :: (Element t, Num t) => [t] -> Matrix t
makeVector = fromLists . transpose . flip (:) [] 

optPoly :: [Double] -> [Double]
optPoly s = map (fromIntegral . round) $ head $ transpose $ toLists $ linearSolve
	(genInputMatrix $ map integral2Double [1 .. length s]) $ 
	makeVector s

seqTerm :: Num a => [a] -> a -> a
seqTerm s x = sum $ zipWith (*) s $ iterate (*x) 1

seqAllTerms s = map ( (seqTerm s) . integral2Double) [1..]

doubleEq :: Double -> Double  -> Bool
doubleEq x y = (10**(-12)) > (abs (x - y))


getFIT ts s = snd $ head $ dropWhile (\(u,v) -> doubleEq u v) $ zip
	(map (seqTerm ts) [1..]) (map (seqTerm s) [1..])

allFITS s = helper s 1
	where helper s x
		| length s == x = []
		| otherwise = (getFIT s (optPoly $ take x $ seqAllTerms s)) : helper s (x+1)

run = sum . map abs . allFITS 

testSeq = [0,0,0,1] :: [Double]
finalSeq = take 11 $ concat $ repeat [1,-1] ::[Double]

main = print $ floor $ run finalSeq

