import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Either
import Data.List
import Data.Ord

isAnagram :: (Ord a) => [a] -> [a] -> Bool
isAnagram [] [] = True
isAnagram _ [] = False
isAnagram [] _ = False
isAnagram (a:as) b
	| elem a b = isAnagram as $ delete a b
	| otherwise = False

findAnagrams :: Ord a => [a] -> [[a]] -> [[a]]
findAnagrams x l = filter (isAnagram x) $ dropWhile (<x) l

findAllAnagrams l = map (flip findAnagrams l) l

squaresInRange n m = takeWhile (<m) $ dropWhile (<n) [x*x | x <- [1..]]

int2dig :: Integral a => a -> [a]
int2dig = reverse . map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

dig2int = sum . zipWith (*) (iterate (*10) 1) . reverse

asSquare s1 s2 = sq
	where numSym = length $ nub $ s1
	      tarLen = length s1
	      sq = findAllAnagrams $ squaresInRange (10^(tarLen-1)) (10^tarLen)



main = do
	result <- parseFromFile csvFile "words.txt"
	let wordList = head $ head $ head $ map rights [[result]]
	let wordAna = reverse $ sortBy (comparing (length . head ) ) $
		filter (\x -> length x > 1) $ map (flip (findAnagrams) wordList) wordList
	print $ length $ head $ head $ wordAna
