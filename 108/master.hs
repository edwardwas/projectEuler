import Useful
import Data.List


divCountSqr :: Integral a => a -> Int
divCountSqr n = product $ map (+1) $ map (\x -> 2* length x) $ group $ primeFactors n

run :: Int -> (Int,Int)
run limit = head $ filter (\(u,v) -> v >2*limit) $ zip [10..] $ map (\n -> divCountSqr (n)) [10..]

main = print $ run (10^3)
