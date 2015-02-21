import Data.Numbers.Primes
import qualified Data.Map.Strict as M

sList :: Integral a => [a]
sList = takeWhile (<5000) $ primes

type WaysMap = M.Map Integer Integer

makeMap :: WaysMap
makeMap = M.fromList $ map (flip (,) 0) [2..sum sList]

findKList :: WaysMap -> Integer -> [Integer]
findKList m p = 0:(M.keys $ M.filter (>0) $ fst $ M.split p m )

handlePrime :: WaysMap -> Integer -> WaysMap
handlePrime m p = foldl (\m' nk -> M.adjust (+ 1) nk m') m $ [p-1 .. sum $ takeWhile (<= p) sList]

ans :: [Integer] -> WaysMap -> Integer
ans p m = sum $ map (flip mod (10^16) . (M.!) m) p

run :: Integer
run = ans sList $ foldl( handlePrime) makeMap sList

main = print run
