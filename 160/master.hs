import Data.List

removeTrailing :: Integral a => a -> a
removeTrailing n
        | mod n 10 == 0 = removeTrailing $ div n 10
        | otherwise = n

fiveLast :: Integral a => a -> a
fiveLast x
        | x < 10^5 = x
        | otherwise = mod x (10^5)

makeNumNice = fiveLast . removeTrailing

f :: Int -> Int
f n = foldl1' (\acc x -> makeNumNice (acc * x)) [1..n]

main = putStrLn $ show $ f (10^7)
