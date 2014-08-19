import Solve
import Helper
import GridParser

import Text.ParserCombinators.Parsec
import Data.Either (either)
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map

file = "sudoku.txt" :: FilePath

tidyString :: String -> String
tidyString s = if last s == '\n' then s else s ++ ['\n']

getGrids :: FilePath -> IO [Grid]
getGrids f = readFile f >>= either (error.show) (return) . parse parseAllGrids "" 

getThreeDigits :: Grid -> [Int]
getThreeDigits g = map (fromJust . (Map.!) g) [(0,0),(1,0),(2,0)]

digsToNum :: Integral a => [a] -> a
digsToNum = sum . zipWith (*) (iterate (*10) 1) . reverse

main = (liftM (sum . map (digsToNum . getThreeDigits . fromJust . solveGrid)) 
		$ getGrids file) >>= print
