import Control.Monad
import Data.List
import System.Environment
import Data.Array

data Square = Black | Red Int deriving (Show,Eq,Ord)

squareLength Black = 1
squareLength (Red x) = x

isBlack Black = True
isBlack _ = False

isRed = not . isBlack

redTouching [] = False
redTouching [x] = False
redTouching (x:y:xs)
    | isRed x && isRed y = True
    | otherwise = redTouching (y:xs)


waysOfMaking 0 = [[]]
waysOfMaking 1 = [[Black]]
waysOfMaking 2 = [[Black,Black]]
waysOfMaking 3 = [Red 3] : map (Black:) (waysOfMaking 2)

waysOfMaking n = do
    s <- Black : map Red [3..n]
    filter (not . redTouching) $ map (s:) (waysOfMaking $ n - squareLength s)

f :: (Integral a) => a -> a -> a
f n m = if n > m then 1 else 1 + sum [f n (m - s - bl - 1) | s <- [0..m-n], bl <- [n..m-s]]

run n = arr ! n
    where arr = array (-n,n) $ zip [(-n)..n] $ map func [(-n)..n]
          func m = if 3 > m then 1 else 1 
                + sum [arr !  (m - s - bl - 1) | s <- [0..m-3], bl <- [3..m-s]]


main = getArgs >>= print . run  . read . head


