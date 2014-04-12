import Data.List
import Data.List.Ordered hiding (union)

sol1 = sum $ union [3,6..999] [5,10..999]
sol2 = sum $ filter (\x -> mod x 2 == 0) $ takeWhile (<=4*10^6) fib
sol3 = maximum $ primeFactors 600851475143
sol4 = maximum $ filter (isPalindrome . digits) [x*y | x <- [100..999],y <- [100..999]]
sol5 = product $ map (\(u,v) -> u^v) $ zip p b
        where a = concatMap (group.sort.primeFactors) [2..20]
              b = map (\y -> maximum $ map length $ filter (\x -> head x == y) a) p
              p = primes 20
sol6 = (sum [1..100]) ** 2 - sum (map (**2) [1..100])
sol7 = last $ take 10001 $ primes (10^6)

fib = 1 : helper [1,1,2]
        where helper l = newTerm :  helper  ( newTerm : l)
                where newTerm = head l + head  (tail l)

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral


primes :: Integral a => a -> [a]
primes n = helper [2..n]
        where helper l
                | p > intSqrt n = l
                | otherwise = p : helper (minus l [p,2*p..n])
                where p = head l

primeFactors 2 = [2]
primeFactors 3 = [3]
primeFactors n
        | f == [] = [n]
        | otherwise = primeFactors (head f) ++ primeFactors ( div n (head f) )
        where f = filter (\x -> mod n x == 0) [2 .. intSqrt n]

isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome l
        | (head l) == (last l) = isPalindrome $ tail $ init l
        | otherwise = False

digits n
        | n < 10 = [n]
        | otherwise = (mod n 10) : (digits $ div n 10)

isInt x = floor x == ceiling x
