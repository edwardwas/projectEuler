fibs :: Integral a => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

sol :: Integral a => [a]
sol = helper $ tail fibs
	where helper (x:y:xs) = x*y : helper xs

main :: IO ()
main = print $ sol !! 14
