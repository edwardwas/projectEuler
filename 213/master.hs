import System.Random


gridSize = 30 :: Integer

unoccupied :: Integral a => a -> [a] -> Int
unoccupied s g = length $ filter (==False) $ map (\x -> elem x g) [1..s*s]

jumpOptions :: Integral a => a -> a -> [a]
jumpOptions s i = filter (<=s*s) $ filter (>0) $ [i+1,i-1,i+30,i-30]

makeJump :: Integral a => a -> (Int,a) -> a
makeJump s (r,i) = options !! (mod r $ length options)	
	where options = jumpOptions s i

evolveGrid rList s g = (rUnused , map (makeJump s) $ zip rUsed g)
	where rUsed = take (s*s) rList
	      rUnused = drop (s*s) rList
