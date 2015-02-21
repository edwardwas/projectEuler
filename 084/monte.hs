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

comunityChest :: Square -> IO Square
comunityChest s = do
	r <- randomRIO (1,16) :: IO Integer
	case r of
		1 -> return GO
		2 -> return JAIL
		_ -> return s

chanceCard :: Square -> IO Square
chanceCard s = do
	r <- randomRIO (1,16) :: IO Integer
	case r of
		1 -> return GO
		2 -> return JAIL
		3 -> return C1
		4 -> return E3
		5 -> return H2
		6 -> return R1
		7 -> return $ nextRailRoad s
		8 -> return $ nextRailRoad s
		9 -> return $ nextUtil s
		10 -> return $ move s (-3)
		_ -> return s

onLand :: Square -> IO Square
onLand s
	| isChance s = chanceCard s
	| isChest s = comunityChest s
	| s == G2J = return JAIL
	| otherwise = return s

sumOfDice :: (Random a, Integral a) => IO a
sumOfDice = (+) <$> randomRIO (1,6) <*> randomRIO (1,6)

move :: Square -> Int -> Square
move start n = toEnum $ mod x (fromEnum (maxBound :: Square))
	where y = n + fromEnum start
	      x = if y < 0 then y + 100 else y

makeNMove :: Int -> Square -> IO [Square]
makeNMove n = sequence . take n . iterate (\x -> liftM2 move x sumOfDice >>= onLand) . return

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
	n <- read . head <$> getArgs :: IO Int
	c <- liftM makeCounts $ makeNMove n GO
	print c
	print $ makeModalString $ map fst $ take 3 c
