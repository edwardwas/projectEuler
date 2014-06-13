module Primes(
isPrime,
modExp
) where 

import Data.List
import System.Random

getSD :: (Integral a) => a -> (a,a)
getSD n = helper (n-1) 0
	where helper x i
		| even x = helper (div x 2) (i+1)
		| otherwise = (i,x)

modExp :: Integral a => a -> a -> a -> a
modExp a 0 _ = 1
modExp a b e 
	| odd b = flip (mod) e $ (mod a e) * (mod (modExp a (b-1) e) e)
	| otherwise = mod (p*p) e
	where p = flip (mod) e $ modExp a (div b 2) e

witness :: Integral a => a -> a -> Bool
witness n a = and $ [(modExp a d n) /= 1] ++ map (\r -> (modExp a (d * (2 ^ r)) n) /= (n-1)) [0..(s-1)]
        where (s,d) = getSD n

makeWitnesses :: (Integral a, Random a, RandomGen r) => a -> Int -> r -> ([a],r)
makeWitnesses n k r = (map fst x, snd $ last x)
        where x = take k $ iterate (\(_,g) -> randomR (1,n-1) g) $ randomR (1,n-1) r

isPrime :: (Integral a, Random a, RandomGen t) => a -> Int -> t -> (Bool,t)
isPrime n k r = (or $ map (witness n) x , g)
        where (x,g) = makeWitnesses n k r
