import Data.Foldable (foldr')
import Data.List (genericReplicate)
import System.Environment (getArgs)
import Control.Monad (join)

modVal = 14 ^ 8 :: Integer

kArrow :: Integer -> Integer -> Integer -> Integer 
kArrow 0 a b = (*) a b
kArrow 1 a b = (flip mod modVal) $ (^) a b
kArrow n a b = foldr' (\x y -> (flip mod modVal) $ kArrow (n-1) x y) a $ genericReplicate (b-1) a

acker 1 1 = 3
acker m n = (kArrow (m-2) 2 (n+3)) - 3

run n = flip mod modVal $ sum $ map (join acker) [1..n]

main = getArgs >>= print . run . read . head


