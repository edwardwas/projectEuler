import Numeric.LinearAlgebra
import Data.Packed.Matrix
import Data.Packed.Vector

gridSize = 30  :: Int

type Flea = (Int,Int)

moveOptions :: Flea -> [Flea]
moveOptions (x,y) = filter f $ [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
	where f (x,y) = and [x>=0,y>=0,x<gridSize,y<gridSize]

stateVector :: Flea -> Float ->  Vector Float
stateVector (a,b) k = flatten $ buildMatrix gridSize gridSize (\pos -> if f pos then val*k else 0)
	where f (x,y) = 1 == ( (abs $ x-a) + (abs $ y-b) )
	      val = (fromIntegral $ length $ moveOptions (a,b)) ** (-1)

startMatrix :: Matrix  Float
startMatrix = ident $ gridSize ^ 2

genAllFleas :: [Flea]
genAllFleas = [(x,y) | x <- [0..gridSize - 1],  y <- [0 .. gridSize - 1]]

transitionMatrix :: Matrix Float -> Matrix Float
transitionMatrix = fromColumns . zipWith (stateVector) (genAllFleas) . getProbs 

doSim :: Int -> [Matrix Float]
doSim n = take (n+1) $ iterate (transitionMatrix) startMatrix

getProbs :: Matrix Float -> [Float]
getProbs = map (sum.toList) . toRows

run = sum  $ map (\x -> 1 - x) $  getProbs $ last $ doSim 50 

main = print $ run

