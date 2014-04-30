nonChanging :: (Eq a) => [a] -> Maybe a
nonChanging (x:[]) = Nothing
nonChanging (x:xs)
	| x == head xs = Just x
	| otherwise = nonChanging xs

hypExp :: Integral a => a -> [a]
hypExp a = scanl1 (\x y -> mod (y^x) (10^8)) $ repeat a

main = print $ nonChanging $ hypExp 1777
