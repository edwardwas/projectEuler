import qualified Data.Map as Map
import Data.List
import Data.List.Split
import System.Environment
import Control.Applicative

import Parser

infinity :: Num a => a
infinity = 999999999999

prettyGrid :: Int -> Grid -> String
prettyGrid s g = unlines $ chunksOf 3 $ concatMap show  $ Map.elems g

isNeigh :: Int -> Int -> Int -> Bool
isNeigh sz a b = and [a >= 0, b >= 0, a < sz*sz, b < sz*sz,
		or [b - a == 1,
		and [div (abs (a-b)) sz == 1, mod (abs (a-b)) sz == 0]]]

weight :: Int -> Grid -> Int -> Int -> Int
weight sz g i j = if isNeigh sz i j then g Map.! i else infinity

shortPath :: Int -> Grid -> Int -> Int -> Int -> Int
shortPath s g i j 0 = weight s g i j
shortPath s g i j k = min (shortPath s g i j (k-1)) $
		(shortPath s g i k (k-1)) + (shortPath s g k j (k-1))

findPath :: (Int,Grid) -> Int -> Int -> Int
findPath (sz,gr) i j = (shortPath sz gr i j (sz*sz)) + (weight sz gr i j)

main = do
	f <- head <$> getArgs
	loadGrid f >>= (\x -> return $ findPath x 0 80) >>= print

