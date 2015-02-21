import System.Environment
import Data.Ratio
import qualified Data.Set as S
import Data.Maybe (mapMaybe,fromJust,isJust)

type Point = (Rational, Rational)
type LineSeg = (Point,Point)

ordNub :: Ord a => [a] -> [a]
ordNub l = helper S.empty l
	where helper _ [] = []
	      helper s (x:xs) = if x `S.member` s then helper s xs
					else x: helper (S.insert x s) xs

genPairings :: [a] -> [(a,a)]
genPairings (x:[]) = []
genPairings (x:xs) = (zip (repeat x) xs) ++ (genPairings xs)

blumBlum :: [Rational]
blumBlum = map (flip (%) 1 . flip mod 500) $ tail $  iterate (flip mod 50515093 . (^2) ) 290797

getM :: LineSeg -> Maybe Rational
getM ((x1,y1),(x2,y2))
	| x1 == x2 = Nothing
	| otherwise = Just $ (y1 - y2) / (x1 - x2)

isInteriorPoint :: Point -> LineSeg -> Bool
isInteriorPoint p (p1,p2) = not $ all (==p) [p1,p2]

isBetween :: Ord a => a -> a -> a -> Bool
isBetween a b c = (a > c && b < c) || (a < c && b > c)

isInLine :: Point -> LineSeg -> Bool
isInLine (a,b) ((x1,y1),(x2,y2)) = isBetween x1 x2 a && isBetween y1 y2 b

makeLineSeg :: [Rational] -> [LineSeg]
makeLineSeg (a:b:c:d:xs) = ( (a,b),(c,d)) : makeLineSeg xs
makeLineSeg _ = []

intersectionPoint :: LineSeg -> LineSeg -> Maybe Point
intersectionPoint l1@((x1,y1),(x2,y2)) l2@((a1,b1),(a2,b2)) 
	| (m1' == Nothing || m2' == Nothing || m1' == m2') = Nothing
	| otherwise = Just (x',y')
	where m1' = getM l1
	      m1 = fromJust m1'
	      m2' = getM l2
	      m2 = fromJust m2'
	      x' = (y1 - m1*x1 - b1 + m2*a1) / (m2 - m1)
	      c = y1 - m1*x1
	      y' = x'*m1 + c

isValid :: LineSeg -> LineSeg -> Maybe Point
isValid l1 l2 
	| isJust p' = if and [isInLine p l1, isInLine p l2, isInteriorPoint p l1, 
		isInteriorPoint p l2, (getM l1) /= (getM l2)] then Just p else Nothing
	| otherwise = Nothing
	where p' = intersectionPoint l1 l2
	      p = fromJust p'

run x = length $ ordNub $ mapMaybe (uncurry isValid) $ genPairings $
		makeLineSeg $ take (4*x) $ blumBlum


main = getArgs >>= (return . read . head) >>= print . run

