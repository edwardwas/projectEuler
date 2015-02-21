import Data.List (delete,concatMap)
import System.Environment
import Control.Monad.Writer

type StepNum = (Integer,[Integer])

createNewStepNum :: StepNum -> [StepNum]
createNewStepNum (l,nu) = map (\x -> (x,delete x nu)) $ steps l

steps :: Integral a => a -> [a]
steps 0 = [1]
steps 9 = [8]
steps n = [n-1,n+1]

updateSteps :: [StepNum] -> [StepNum]
updateSteps = concatMap createNewStepNum

makeStartingSteps :: [StepNum]
makeStartingSteps = map (\x -> (x,delete x [0..9])) [1..9]

run :: Int -> Int
run n = fst $ helper n (0,makeStartingSteps)
	where helper :: Int -> (Int,[StepNum]) -> (Int,[StepNum])
	      helper 0 x = x
	      helper n (tot,st) = helper (n-1) (newTot,newSt)
		where newSt = updateSteps st
		      newTot = tot + (length $ filter isPan $ newSt)

run' :: Int -> [StepNum] -> Writer (Sum Int) [StepNum]
run' n st = do
	let newSt = updateSteps st
	tell $ Sum $ length $ filter isPan $ newSt
	if n == 1 then
		return newSt
	else run' (n-1) newSt
	
	
isPan :: StepNum -> Bool
isPan (_,[]) = True
isPan _ = False

main = getArgs >>= print . snd . runWriter . flip run' makeStartingSteps . read . head


