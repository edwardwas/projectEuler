import Control.Monad

type TempPair = (Float, [Int]) -- Acc,List

update :: Int -> TempPair -> [TempPair]
update m (acc,l)
        | acc == 0.5 = [(acc,l)]
        | acc > 0.5 = []
        | otherwise = map (\n -> (acc+(invSqr n),n:l)) $ enumFromTo ((+1) $ head l) m

doStep :: Int -> [TempPair] -> IO [TempPair]
doStep m l = (print $ (show $ length l) ++ " operations to do..") >> (return $ concat $ map (update m) l)

powerSeries :: [a] -> [[a]]
powerSeries = init . filterM (const [True,False])

invSqr :: (Integral a, Floating b) => a -> b
invSqr = (** (-2)) . fromIntegral

doWhileChange :: Eq a => (a -> IO a) -> a -> IO a
doWhileChange f x = (f x) >>= (\n -> if n == x then return x else doWhileChange f n)

main = doWhileChange (doStep 45) [(0.25,[2])] >>= print

