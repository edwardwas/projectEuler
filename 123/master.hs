import Data.Numbers.Primes

f :: Integral a => a -> a -> a
f n p = if even n then 2 else 2*p*n

run :: Integral a => a -> a
run t = fst $ head $ filter (\x -> snd x > t) $
        map (\(n,p) -> (n,f n p)) $ zip [1..] primes

main = print $ run $ 10^10
