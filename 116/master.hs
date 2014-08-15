import System.Environment
import Data.Array

fit :: (Integral a, Ix a) => a -> a -> a
fit rl tl = helper rl
	where helper x
		| x < tl = 0
		| x == tl = 1
		| otherwise = sum $ map ((+) 1 . (!) dp . (-) (x - tl)) [0..(x-tl)]
		where dp = array (0,rl+1) [(y, helper y) | y <- [0..rl+1]]

main = do
	args <- getArgs
	let n = read (head args) :: Integer
	print $ sum $ map (fit n) [2,3,4]
