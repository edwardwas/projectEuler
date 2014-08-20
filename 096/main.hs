import Solve
import Helper
import GridParser

import Text.ParserCombinators.Parsec (parse)
import Data.Either (either)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import System.Environment (getArgs)

tidyString :: String -> String
tidyString s = if last s == '\n' then s else s ++ ['\n']

getGrids :: FilePath -> IO [Grid]
getGrids f = readFile f >>= either (error.show) (return) . parse parseAllGrids "" . tidyString

getThreeDigits :: Grid -> [Int]
getThreeDigits g = map (fromJust . (Map.!) g) [(0,0),(1,0),(2,0)]

digsToNum :: Integral a => [a] -> a
digsToNum = sum . zipWith (*) (iterate (*10) 1) . reverse

run :: FilePath -> IO Int
run = liftM (sum . map (digsToNum . getThreeDigits . fromJust . solveGrid)) . getGrids 

main :: IO ()
main = getArgs >>= run . head  >>= print
