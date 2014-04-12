
s n r = sum $ map ( \x -> (900-3*r)*(r**(x-1) ) ) [1..n]

binSearch :: (Double -> Double) -> Double-> Double -> [(Double,Double,Double)]
binSearch f high low
        | high - low < (10 ** (-13)) = [out]
        | ans < 0 = out : binSearch (f) high mid
        | ans > 0 = out : binSearch (f) mid low
        where ans = f mid
              mid = (high + low) * 0.5
              out = (high,mid,low)

toSolve r = (6*10**11) + s 5000 r

out = binSearch (toSolve) 1.001999000000 1.002999000000

main = putStrLn $ show $ out

