{-# LANGUAGE ExistentialQuantification #-}

import System.Environment
import Data.List (nub)
import Data.Maybe
import qualified Data.Set as S


blumBlum :: Integral a => [a]
blumBlum = map (flip mod 500) $  iterate (flip mod 50515093 . (^2) ) 290797

type Point = (Double,Double)
type LineSeg = (Point,Point)

intersectionPoint :: LineSeg -> LineSeg -> Point
intersectionPoint ((x1,y1),(x2,y2)) ((a1,b1),(a2,b2)) = (x',y')
	where m1 = (y1 - y2) / (x1 - x2)
	      m2 = (b1 - b2) / (a1 - a2)
	      x' = (y1 - m1*x1 - b1 + m2*a1) / (m2 - m1)
	      c = y1 - m1*x1
	      y' = x'*m1 + c

isInteriorPoint :: Point -> LineSeg -> Bool
isInteriorPoint p (p1,p2) = not $ all (==p) [p1,p2]

isBetween :: Ord a => a -> a -> a -> Bool
isBetween a b c = (a > c && b < c) || (a < c && b > c)

isInLine :: Point -> LineSeg -> Bool
isInLine (a,b) ((x1,y1),(x2,y2)) = isBetween x1 x2 a && isBetween y1 y2 b

isValid :: LineSeg -> LineSeg -> Maybe Point
isValid l1 l2 = if and [isInLine p l1, isInLine p l2, isInteriorPoint p l1, 
		isInteriorPoint p l2] then Just p else Nothing
	where p = intersectionPoint l1 l2

ordNub :: Ord a => [a] -> [a]
ordNub l = helper S.empty l
	where helper _ [] = []
	      helper s (x:xs) = if x `S.member` s then helper s xs
					else x: helper (S.insert x s) xs

genPairings :: [a] -> [(a,a)]
genPairings (x:[]) = []
genPairings (x:xs) = (zip (repeat x) xs) ++ (genPairings xs)

makeLineSeg :: [Double] -> [LineSeg]
makeLineSeg (a:b:c:d:xs) = ( (a,b),(c,d)) : makeLineSeg xs
makeLineSeg _ = []

main = getArgs >>= (return . read . head) >>= print . run
	
run x = length $ ordNub $ mapMaybe (uncurry isValid) $ genPairings $ 
	makeLineSeg $ take (4*x) $ tail $ map (fromIntegral) blumBlum
