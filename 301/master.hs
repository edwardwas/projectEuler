import Data.Bits
import Data.List

toBinary :: Integral a => a -> [a]
toBinary = reverse . map (flip (mod) 2 ) . takeWhile (> 0) . iterate (flip (div) 2) 

xorSum :: Bits a => [a] -> a
xorSum (l:ls) = foldl (\acc x -> xor acc x) l ls

padToSame :: Num a => [[a]] -> [[a]]
padToSame x = map (\y -> (replicate (maxL - length y) 0) ++ y) x
	where maxL = maximum $ map length x

nimSum :: (Bits a, Integral a) => [a] -> a
nimSum = sum . map xorSum . transpose . padToSame . map toBinary 

run :: Int -> Int
run x = length $ filter (==0) $ map (\n -> nimSum [n,2*n,3*n]) [2..x+1]
--Everything Above Here is Brute force. Below is the clever implementation
--I left it here becuase it is used to spot the pattern. And it is quite
--pretty.

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)
main = print $ last $ take 32 $ fib

