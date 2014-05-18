figurate :: Integral a => a -> [a]
figurate n = takeWhile (<10^4) $ dropWhile (<10^3) $ scanl1 (+) [1,(n-1)..]

splitNum :: Integral a => a -> (a,a)
splitNum n = (div n 100, mod n 100)

fList :: [Integer] -> [[(Integer,Integer)]]
fList = map (\x -> zip (repeat x) $ figurate x) 

isNextCyc :: Integral a => a -> a -> Bool
isNextCyc a b = (snd $ splitNum a) == (fst $ splitNum b)

chooseNext :: [[(Integer,Integer)]] -> Int -> [(Integer,Integer)] -> [(Integer,Integer)]
chooseNext f n x = if length x == n then []
	else filter (isNextCyc a . snd) $ filter (not . flip (elem) done . fst) $ concat f
	where a = snd $ head x
	      done = map fst x

getList :: [[(Integer,Integer)]] -> Int ->  [[(Integer,Integer)]] -> [[(Integer,Integer)]]
getList f n x = if y == x then x else getList f n y
	where y = concatMap (helper f) x
	      helper f x = if k == [] then [x] else map (\z -> (z:x)) k
		where k = chooseNext f n  x

isListCyc :: Integral a => [(a,a)] -> Bool
isListCyc x = isNextCyc (snd $ head x) (snd $ last x)

run l = filter (isListCyc) $ filter (\x -> length x == n) $ concat 
	$ map (\z -> getList f n [[z]]) $ head f
	where f = fList l
	      n = length l


main = print $ sum $ map snd $ head $ run [3,4,5,6,7,8]
