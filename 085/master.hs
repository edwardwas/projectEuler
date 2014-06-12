
numWays n m x y = (m-y+1)*(n-x+1)

numRects n m = sum $ [numWays n m x y | x <- [1..n], y <-[1..m]]

closest t l = foldl (\x acc -> if abs( x -t) < abs(t - acc) then x else t) l 10^6
