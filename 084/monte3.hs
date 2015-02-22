import System.Random
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Data.List
import System.Environment

data Square = GO | A1 | CC1 | A2 | T1 | R1 | B1 | CH1 | B2 | B3 | JAIL
	| C1 | U1 | C2 | C3 | R2 | D1 | CC2 | D2 | D3 | FP
	| E1 | CH2 | E2 | E3 | R3 | F1 | F2 | U2 | F3 | G2J
	| G1 | G2 | CC3 | G3 | R4 | CH3 | H1 | T2 | H2
	deriving (Eq,Show,Enum,Bounded,Ord)

isChance :: Square -> Bool
isChance = (==) "CH" . take 2 . show

isChest :: Square -> Bool
isChest = (==) "CC" . take 2 . show

isRail:: Square -> Bool
isRail = (==) 'R' . head  . show

isUtil :: Square -> Bool
isUtil = (==) 'U' . head  . show

nextRailRoad :: Square -> Square
nextRailRoad s
	| isRail s = s
	| otherwise = nextRailRoad $ move s 1

nextUtil :: Square -> Square
nextUtil s
	| isUtil s = s
	| otherwise = nextUtil $ move s 1

move :: Square -> Int -> Square
move start n = toEnum $ mod x (fromEnum (maxBound :: Square))
	where y = n + fromEnum start
	      x = if y < 0 then y + 100 else y

comunityChest :: Int -> Square -> Square
comunityChest 1 s = GO
comunityChest 2 s = JAIL
comunityChest _ s = s

chanceCard :: Int -> Square -> Square
chanceCard 1 s = GO
chanceCard 2 s = JAIL
chanceCard 3 s = C1
chanceCard 4 s = E3
chanceCard 5 s = H2
chanceCard 6 s = R1
chanceCard 7 s = nextRailRoad s
chanceCard 8 s = nextRailRoad s
chanceCard 9 s = nextUtil s
chanceCard 10 s = move s (-3)
chanceCard _ s = s

----------------------------

dieRoll :: RandomGen g => Int -> g -> (Int,Bool,g)
dieRoll n g = (a+b,a==b,g'')
	where (a,g') = randomR (1,n) g
	      (b,g'') = randomR (1,n) g'

onLand :: Int -> Square -> Square
onLand a s
	| isChance s = chanceCard a s
	| isChest s = comunityChest a s
	| s == G2J = JAIL
	| otherwise = s

----------------------------------------------------

data SimState = SimState {
	  squaresVisited :: [Square]
	, conseqDoubles :: Int
	, gen :: StdGen
	, diceSides :: Int
	} deriving (Show)


makeMove :: Square -> State SimState Square
makeMove s = do
	oldState <- get
	let (dr,doub,g) = dieRoll (diceSides oldState) (gen oldState)
	let (a,g') = randomR (1,16) g :: (Int,StdGen)
	let newConseqDoubles = if doub then conseqDoubles oldState + 1 else 0
	let newSqr = if newConseqDoubles == 3 then JAIL else onLand a $ move s dr
	put $ SimState (s : squaresVisited oldState) (mod newConseqDoubles 3) g' (diceSides oldState)
	return newSqr

startState :: Int -> StdGen -> SimState
startState n g = SimState [] 0 g n

doMany :: Monad m => Int -> (a -> m a) -> a -> m a
doMany 0 _ s = return s
doMany n f s = f s >>= doMany (n-1) f

runSim :: Int -> SimState -> [Square]
runSim n start = squaresVisited $  snd $ flip runState start $ doMany n makeMove GO

runIO :: Int -> IO [Square]
runIO n = liftM (runSim n . startState 4) newStdGen

---------


sndCompare x y = compare (snd x) (snd y)

makeCounts :: Ord a => [a] -> [(a,Int)]
makeCounts = sortBy (flip sndCompare) . map (head &&& length ) . group . sort

numToString :: Int -> String
numToString x
	| x < 10 = '0' : show x
	| otherwise = show x

makeModalString :: [Square] -> String
makeModalString = concatMap (numToString . fromEnum)

main = do
	a <- liftM makeCounts $ getArgs >>= runIO . read . head 
	print a
	print $ makeModalString $ take 3 $ map fst a
