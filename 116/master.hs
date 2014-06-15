import System.Environment
import Data.Array

fitings rl tl
        | tl > rl = 0
        | tl == rl = 1
        | otherwise = sum $ map ((+) 1 . flip (fitings) tl . (-) (rl - tl)) [0..(rl-tl)]

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
