import Data.List

isPalin :: (Eq a) => [a] -> Bool
isPalin [] = True
isPalin (x:[]) = True
isPalin x
        | (head x) == (last x) = isPalin $ tail $ init x
        | otherwise = False

num2digs :: Integral a => a -> a -> [a]
num2digs b = reverse . map (flip (mod) b) . takeWhile (>0) . iterate (flip (div) b)

digits2num :: Integral a => a -> [a] -> a
digits2num b = sum . zipWith (*) (iterate (*b) 1) . reverse

addAll :: [a] -> [[a]] -> [[a]]
addAll x y = [a ++ [b] | a <- y, b <- x]

allPalinGerms :: (Integral a) => Int -> [[a]]
allPalinGerms l = concat $ take l $ iterate (addAll [0..9]) $ map (flip (:) []) [1..9]

palinFromGerm :: [a] -> [[a]]
palinFromGerm (x:[]) = [[x],[x,x]]
palinFromGerm x = [ x ++ (reverse x) ] ++ [ (init x) ++ [last x] ++ (reverse $ init x)]

isValid :: [Integer] -> Bool
isValid = isPalin . num2digs 2 . digits2num 10

run :: Int -> Integer
run = sum . map (digits2num 10) . filter (isValid) . concatMap (palinFromGerm) . filter (odd . head) .  allPalinGerms 

main = print $ run 3
