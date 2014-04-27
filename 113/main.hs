fact :: Integral a => a -> a
fact n = product [1..n]

binomial :: Integral a => a -> a -> a
binomial n k = div (fact n) ( (fact k) * (fact (n-k) ) )

nonBouncyBelow :: Integral a => a -> a
nonBouncyBelow i = (binomial (8+i) 8) + (binomial (9+i) 9) - 10

main = print $  sum $ map nonBouncyBelow [1..100]
