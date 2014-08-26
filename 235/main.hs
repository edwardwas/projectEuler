fun_S :: (Floating a,Enum a) => a -> a
fun_S r = (+) (600000000000) $ sum $ map (flip fun_U r) $ enumFromTo 1 5000
        where fun_U k = (*) (900 - 3*k) . flip (**) (k-1)

solver :: (Floating a, Eq a) => (a -> a) -> (a, a) -> (a,a)
solver f st = if st == end then st else solver f end
        where end = helper f st
              helper f (a,b) = if ( (signum $ f a) == (signum $ f c)) then (c,b) else (a,c)
                where c = 0.5*(a+b)

cleanUp :: (Floating a, RealFrac a) => a ->  (a,a) -> a
cleanUp r = toPrec r . (* 0.5) . uncurry (+)

toPrec :: (Floating a, RealFrac a) => a -> a -> a
toPrec r = (/ e) . fromIntegral . round . (* e)
        where e = 10**r

main = (print) $ cleanUp 12 $ solver (fun_S) (1.003,1.00)
