import System.Environment
import Control.Monad

findMinDistance2 :: Int -> Int -> Int -> Int
findMinDistance2 l w h = l*l + (w+h)*(w+h)

isSquare :: Int -> Bool
isSquare x = (fromIntegral $ floor y) == y
	where y = sqrt $ fromIntegral x

newOnly :: Int -> Int
newOnly m = length $ filter isSquare $
	[findMinDistance2 m b c | b <- [1..m], c <- [1..m], b >= c]

calculateAll :: [Int]
calculateAll = scanl (+) 0 $ map newOnly [1..]

run :: Int -> (Int,Int)
run x = head $ dropWhile ( (<x) . snd) $ zip [0..] calculateAll

main = print $ run $ 10^6
