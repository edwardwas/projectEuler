import Data.List

fact :: Integral a => a -> a
fact n = product [1..n]

count f = length . filter f

biNomCoeff :: Integral a => a -> a -> a
biNomCoeff n k = div (fact n) ((fact k) * (fact (n-k) ) )

toBase b n = map (flip (mod) b) $ takeWhile (>0) $ iterate (flip (div) b) n

divSeven  n k = (count f [(n-k+1)..n]) - (count f [1..k]) 
	where f x = mod x 7 == 0
