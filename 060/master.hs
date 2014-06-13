import qualified Data.Numbers.Primes as Primes
import Data.List.Ordered
import Data.List
import System.Environment

isPrime :: Integer -> Integer -> Bool
isPrime start x = member x $ dropWhile (< start) $ Primes.primes

dig2num :: Integral a => [a] -> a
dig2num = sum . zipWith(*) (iterate (*10) 1) 

num2dig :: Integral a => a -> [a]
num2dig = map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

catNum :: Integral a => a -> a -> [a]
catNum x y = map dig2num $ [a++b,b++a ]
	where a = (num2dig x)
	      b = (num2dig y) 

allCat :: Integral a => [a] -> [a]
allCat x = concat $ helper x
	where   helper (_:[]) = []
		helper (x:xs) = (helper xs) ++ (map (catNum x) xs)

pairWith pl x = filter (all (isPrime x) . allCat) $ [ [x,b] | b <- dropWhile (<= x) $ tail pl]

addPair pl x = [ x ++ [b] | b <- map last $ pairWith pl $ last x 
		, all (isPrime $ head x) $ allCat $ (init x) ++ [b]]

run pl i n = map sum $ last $ take (i-1) $ iterate (concatMap (addPair pl)) $ addPair pl [n]

main = do 
	args <- getArgs
	let len = read (head args) :: Int
	let start = read (head $ tail args) :: Integer
	print $ run (takeWhile (<1000) $ Primes.primes) len start




