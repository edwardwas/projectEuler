module Parser where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many)
import qualified Data.Map as Map

type Grid = Map.Map Int Int

gridFromList :: [[Int]] -> (Int,Grid)
gridFromList l = (length l, Map.fromList $ zip [0..] $ concat l)

parseLine :: Parser [Int]
parseLine = (map read) <$> sepBy (many1 digit) (oneOf ",") 

parseFile :: Parser (Int,Grid)
parseFile = (makeGrid . filter (not . null)) <$> (sepBy parseLine $ newline)

isValidGrid :: [[Int]] -> Bool
isValidGrid l
	| length l /= (length $ head l) = False
	| otherwise = True

makeGrid :: [[Int]] -> (Int,Grid)
makeGrid l = if isValidGrid l then gridFromList l else error "Invalid Grid"

loadGrid :: FilePath -> IO (Int,Grid)
loadGrid f = readFile f >>= either (error.show) (return) . parse parseFile ""
