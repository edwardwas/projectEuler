
hypExp a 1 = a
hypExp a k = mod (a ^ (hypExp a (k-1))) (10^8)

hypExp' a = scanl1 (\x y -> mod (y^x) (10^8)) $ repeat a

main = print $ last $ take 1855 $ hypExp' 1777 
