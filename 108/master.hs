import Useful
import Data.List

divCountSqr :: Integral a => a -> Int
divCountSqr = product . map (+1) . map ((*2) . length) . group . primeFactors 

run :: Int -> (Int,Int)
run limit = head $ filter (\(u,v) -> v >2*limit) $ zip [10..] $ map  divCountSqr  [10..]

main = print $ run (10^3)
