import Data.List
import System.Environment


nextStep :: Integral a => a -> [a]
nextStep 0 = [1]
nextStep 9 = [8]
nextStep n = [n-1,n+1]

extendNumber :: Integral a => [a] -> [[a]]
extendNumber l@(x:_) = map (\y -> y:l) $ nextStep x

isPandigital :: Integral a => [a] -> Bool
isPandigital n = helper [0..9] n
        where helper [] ys = True
              helper xy [] = False
              helper xs (y:ys) = helper (delete y xs) ys

allStepNums :: Integral a => Int -> [[a]]
allStepNums n = concat $ take n $ iterate (concatMap extendNumber) $ map helper [0..9]
        where helper x = [x]

main :: IO ()
main = getArgs >>= print . length . filter isPandigital . allStepNums . read . head
