import Data.Ratio
import Data.Maybe
import Data.Set as S
import Control.Applicative

type Point = (Rational, Rational)
type MCPair = (Maybe Rational, Maybe Rational)
type LineSeg = (Point,Point,MCPair)

ordNub :: Ord a => [a] -> [a]
ordNub l = helper S.empty l
	where helper _ [] = []
	      helper s (x:xs) = if x `S.member` s then helper s xs
					else x: helper (S.insert x s) xs

makeMC :: (Point,Point) -> MCPair
makeMC ((x1,y1),(x2,y2)) = (m,c)
	where m = if (x1 == x2) then Just ( (y1 - y2) / (x1 - x2)) else Nothing
	      c = (y1 - ) . (*x1) <$> m

makeLineSeg :: (Point,Point) -> LineSeg
makeLineSeg p@(p1,p2) = (p1,p2,makeMC p)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween a b n = (a > n && b < n) || (a < n && b > n)

pointOnLine :: LineSeg -> Point -> Bool
pointOnLine ((x1,y1),(x2,y2),_) (x,y) = isBetween x1 x2 x && isBetween y1 y2 y

intersectionPoint :: LineSeg -> LineSeg -> Maybe Point
intersectionPoint ((x',y'),_,(m1,c1)) :wq

	where findX m1 c1 m2 c2 = (c1 - c2) / (m2 - m1)
	      findY y m x = y - m*x
