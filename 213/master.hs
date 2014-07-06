import System.Random
import Data.List

gridSize = 30 :: Integer

type Flea = (Integer,Integer)

isValid :: Flea -> Bool
isValid (x,y) = and [x >= 0, y >= 0, x < gridSize, y <gridSize]

addVec :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
addVec (ax,ay) (bx,by) = (ax+bx,ay+by)

getOptions :: Flea -> [Flea]
getOptions f = filter (isValid) $ map (addVec f) $ [(0,-1),(0,1),(1,0),(-1,0)]

choice :: [a] -> StdGen -> (a,StdGen)
choice l g = (c,newG)
        where (i,newG) = randomR (0, length l - 1) g :: (Int,StdGen)
              c = l !! i

makeJump :: Flea -> StdGen -> (Flea,StdGen)
makeJump f g = choice (getOptions f) g

updateGrid :: [Flea] -> StdGen -> ([Flea],StdGen)
updateGrid f g = (map fst l, snd $ last l)
        where l = scanl (\(_,r) x -> makeJump x r) (head f,g) $ tail f

startGrid :: Integer -> [Flea]
startGrid s = [(x,y) | x <- [0..s-1],y<-[0..s-1]]

unoccupied :: Integer -> [Flea] -> Int
unoccupied s l = length $ foldl (\x f -> delete f x) (startGrid s) l

iterateGrid :: ([Flea],StdGen) -> ([Flea],StdGen)
iterateGrid (f,g) = updateGrid f g

runSim s = do
        g <- newStdGen
        return $ unoccupied s $ fst $ last $ take 50 $ iterate iterateGrid $ (startGrid s,g)

main = do
        a <- runSim 30
        print a
