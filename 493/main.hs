import Control.Monad
import System.Random
import Data.List
import System.Environment

data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Show, Eq)

instance Enum Colour where
	toEnum 0 = Red
	toEnum 1 = Orange
	toEnum 2 = Yellow
	toEnum 3 = Green
	toEnum 4 = Blue
	toEnum 5 = Indigo
	toEnum 6 = Violet
	fromEnum Red = 0
	fromEnum Orange = 1
	fromEnum Yellow = 2
	fromEnum Green = 3
	fromEnum Blue = 4
	fromEnum Indigo = 5
	fromEnum Violet = 6

startUrn :: [Colour]
startUrn = concat $ map (replicate 7 . toEnum) [0..6]

numResults :: [Colour] -> Int
numResults = length . nub

addChoice :: (Int,Int) -> [Int] -> IO [Int]
addChoice r a = randomRIO r >>= (\x -> return (x:a))

createNChoices :: Int -> (Int,Int) -> IO [Int]
createNChoices n r = helper n r []
	where helper n r x
		| length x == n = return x
		| otherwise = addChoice r x >>= helper n r . nub

runSim :: Int -> [Colour] -> IO Int
runSim n opts = createNChoices n (0,(length opts) - 1) >>= return . numResults .map ((!!) opts)

mean :: [Int] -> Double
mean x = (fromIntegral $ sum x) / (fromIntegral $ length x)

run :: Int -> IO Double
run n = liftM mean $ (replicateM n) $ runSim 20 startUrn

main :: IO ()
main = getArgs >>= run . read . head >>= print


