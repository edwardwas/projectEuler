import Data.List
import System.IO

data Numeral = I | IV | V | IX | X | XL | L | XC | C | LD | D | LM | M deriving (Show, Eq, Ord, Read)
type RomanNum = [Numeral]

decodeNumeral :: Num a => Numeral -> a
decodeNumeral I = 1
decodeNumeral IV = 4
decodeNumeral V = 5
decodeNumeral IX = 9
decodeNumeral X = 10
decodeNumeral XL = 40
decodeNumeral L = 50
decodeNumeral XC = 90
decodeNumeral C = 100
decodeNumeral LD = 400
decodeNumeral D = 500
decodeNumeral LM = 900
decodeNumeral M = 1000


char2Numeral :: Char -> Numeral
char2Numeral = read . flip (:) []

string2latin :: (Num a, Ord a) => String -> a
string2latin = sum . helper . map (decodeNumeral . char2Numeral)
        where helper (x:y:z)
                | x < y = (-x):(helper (y:z))
                | otherwise = x : (helper (y:z))
              helper (x:[]) = [x]

latin2roman :: (Num a, Ord a) => a -> RomanNum
latin2roman x
        | x >= 1000 = M:(latin2roman $ x - 1000)
        | x >= 900 = LM:(latin2roman $ x - 900)
        | x >= 500 = D:(latin2roman $ x - 500)
        | x >= 400 = LD:(latin2roman $ x - 400)
        | x >= 100 = C:(latin2roman $ x - 100)
        | x >= 90 = XC:(latin2roman $ x - 90)
        | x >= 50 = L:(latin2roman $ x - 50)
        | x >= 40 = XL:(latin2roman $ x - 40)
        | x >= 10 = X:(latin2roman $ x- 10)
        | x >= 9 = IX:(latin2roman $ x - 9)
        | x >= 5 = V:(latin2roman $ x - 5)
        | x >= 4 = IV:(latin2roman $ x - 4)
        | x >= 1 = I:(latin2roman $ x - 1)
        | otherwise = []

roman2latin :: (Num a, Ord a) => RomanNum -> a
roman2latin = sum . map decodeNumeral 

roman2string :: RomanNum -> String
roman2string = concat . reverse . sort . map show

run x = (length x) - (length newX)
        where newX = roman2string $ latin2roman $ string2latin x

main = do
        withFile "roman.txt" ReadMode (\handle -> do
                contents <- hGetContents handle
                print $ sum $ map run $ lines contents)
