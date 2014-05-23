import Data.Numbers.Primes

run :: Integral a => a -> a
run t = fst $ head $ 
        dropWhile(\(n,p) -> 2*n*p< t) $ 
        dropAlt $ zip [1..] primes
        where dropAlt (x1:_:xs) = x1 : dropAlt xs

main = print $ run $ 10^10
