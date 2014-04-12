
digits :: Int-> [Int]
digits n  
        | n < 10 = [n]
        | otherwise = b : digits a
        where (a,b) = divMod n 10

freq :: Eq a => a -> [a] -> Int
freq n l  = length $ filter (==n) l

f :: Int -> Int -> Int
f 0 d = 0
f n d = (f (n-1) d) + freq (d) (digits n)


genF :: Int -> [Int]
genF d = 0 : helper d 1 [0]
        where helper d n l = (head new_l) : helper d (n+1) new_l
                where new_l = (freq d (digits n) ) + head l : l

useful _ [] = []
useful [] _ = []
useful (x:xs) (y:ys)
        | x == y = x : useful xs ys
        | otherwise = useful xs ys

