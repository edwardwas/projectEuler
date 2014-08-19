module GridParser where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Helper


line :: Parser String
line = many (alphaNum <|> char ' ')  >>= (\x -> newline >> return x)

parseGrid :: Parser Grid
parseGrid = line >> (count 9 $ line) >>= (return . gridFromString)

parseAllGrids :: Parser [Grid]
parseAllGrids = many parseGrid 

charToGridPoint :: Char -> (Maybe Int)
charToGridPoint '0' = Nothing
charToGridPoint x = Just $ read [x]

gridFromString :: [String] -> Grid
gridFromString = Map.fromList . zip (allPairs [0..8] [0..8]) . map charToGridPoint . concat

