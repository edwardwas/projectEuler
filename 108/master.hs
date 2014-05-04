import Useful
import Data.List

divCountSqr :: Integral a => a -> Int
divCountSqr = product . map (+1) . map ((*2) . length) . group . primeFactors 

run :: Int -> Int
run limit = head $ filter (2*limit<) $ map (divCountSqr)  [10..]

main = print $ run (10^3)
