import Data.List
import Data.Array
import Data.List.Ordered hiding (union)
import Data.Char
import System.IO
import qualified Data.Map as Map

sol1 = sum $ union [3,6..999] [5,10..999]
sol2 = sum $ filter (even) $ takeWhile (<=4*10^6) fib
sol3 = maximum $ primeFactors 600851475143
sol4 = maximum $ filter (isPalindrome . digits) [x*y | x <- [100..999],y <- [100..999]]
sol5 = product $ zipWith (^) p b
        where a = concatMap (group.sort.primeFactors) [2..20]
              b = map (\y -> maximum $ map length $ filter ((== y) . head) a) p
              p = primes 20
sol6 = (sum [1..100]) ** 2 - sum (map (**2) [1..100])
sol7 = last $ take 10001 $ primes (10^6)
sol8 = do
        contents <- readFile "008.txt"
        let a = maximum $ map product $ breakToFive $ map digitToInt contents
        print a
sol10 = sum $ primes (2*10^6)
sol12 = head $ dropWhile ((< 500) . numDivisors) triNum


fib = 1 : helper [1,1,2]
        where helper l = newTerm :  helper  ( newTerm : l)
                where newTerm = head l + head  (tail l)

breakToFive :: [a] -> [[a]]
breakToFive (a:b:c:d:e:xs) = [[a,b,c,d,e]] ++ breakToFive (b:c:d:e:xs)
breakToFive x = [x]

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l

primeFactors :: Integral a => a -> [a]
primeFactors 2 = [2]
primeFactors 3 = [3]
primeFactors n
        | f == [] = [n]
        | otherwise = primeFactors (head f) ++ primeFactors ( div n (head f) )
        where f = filter (\x -> mod n x == 0) [2 .. intSqrt n]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome l
        | (head l) == (last l) = isPalindrome $ tail $ init l
        | otherwise = False

digits :: Integral a => a -> [a]
digits n
        | n < 10 = [n]
        | otherwise = (mod n 10) : (digits $ div n 10)

isInt :: RealFrac a => a -> Bool
isInt x = floor x == ceiling x

numDivisors :: Integral a => a -> Int
numDivisors = product . map (+1) . map length . group . primeFactors

triNum :: Integral a => [a]
triNum = scanl (+) 1 [2..]

collatzMem = listArray (1,n) $ map collatz [1..n]
        where n = (10^6)

collatz :: Integer -> Integer
collatz 1 = 1
collatz n
        | inRange (bounds collatzMem) m = 1 + collatzMem ! m
        | otherwise = 1 + collatz m
        where m = case n of
                1 -> 1
                n | even n -> div n 2
                  | otherwise -> 3 * n +  1
