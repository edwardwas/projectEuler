import System.Random
import Control.Applicative
import Control.Monad
import Data.List
import Control.Arrow
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

onLand :: Int -> Square -> Square
onLand a s
	| isChance s = chanceCard a s
	| isChest s = comunityChest a s
	| s == G2J = JAIL
	| otherwise = s

move :: Square -> Int -> Square
move start n = toEnum $ mod x (fromEnum (maxBound :: Square))
	where y = n + fromEnum start
	      x = if y < 0 then y + 100 else y

sndCompare x y = compare (snd x) (snd y)

makeCounts :: Ord a => [a] -> [(a,Int)]
makeCounts = sortBy (flip sndCompare) . map (head &&& length ) . group . sort

numToString :: Int -> String
numToString x
	| x < 10 = '0' : show x
	| otherwise = show x

makeModalString :: [Square] -> String
makeModalString = concatMap (numToString . fromEnum)

sumOfDice :: (Integral a, Random a,RandomGen b) => a -> b -> (a,b)
sumOfDice n g = (a+b,g'')
	where (a,g')  = randomR (1,n) g
	      (b,g'') = randomR (1,n) g'

diceList :: (Integral a, Random a, RandomGen b) => a -> b -> [a]
diceList a g = map fst $ tail $ iterate (\(_,r) -> sumOfDice a r) (0,g)

cardList n = randomRs (1,n)

randomMoves :: (Int,Int) -> Square -> Square
randomMoves (d,c) s =  onLand c $ move s d

doNMove :: Int -> Int -> Square -> StdGen -> [Square]
doNMove d n start g = scanl (flip randomMoves) start $ take n $ zip (diceList d gA) (cardList d gB)
	where (gA,gB) = split g

main = do
	n <- liftM (read . head) getArgs :: IO Int
	g <- newStdGen
	let a = take 3 $ makeCounts $ doNMove 4 n GO g
	print a
	print $ makeModalString $ map fst a
