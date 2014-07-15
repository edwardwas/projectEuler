import Data.Ratio

type ContFract = ([Integer],[Integer])

invert :: Integral a => Ratio a -> Ratio a
invert r = (denominator r) % (numerator r)

contFract2List :: ContFract -> [Ratio Integer]
contFract2List (a,b) = map (\x -> x % 1) $ a ++ (concat $ repeat b)

calContFract :: Int -> ContFract -> Ratio Integer
calContFract n = invert . foldr (\x y -> invert $ (+) x y) (0%1) . take n . contFract2List

eContFract :: ContFract
eContFract = ([2],concat $ map (\k -> [1,k,1]) [2,4..])

digSum :: Integral a => a -> a
digSum = sum . map (flip (mod) 10) . takeWhile (>0) . iterate (flip (div) 10)

answer = digSum $ numerator $ calContFract 100 $ eContFract

main = print $ answer
