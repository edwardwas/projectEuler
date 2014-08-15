import Data.List

type Fract = (Integer,Integer)

makeFract :: Integer -> Integer -> Fract
makeFract = (,)

canReduce :: Fract -> Bool
canReduce (a,b) = (gcd a b) /= 1

listToFract :: [Integer] -> Fract
listToFract x = makeFract (head x) (last x)

isLessThanOne :: Fract -> Bool
isLessThanOne (a,b) = a < b

genAllFracts :: [Integer] -> [Fract]
genAllFracts = map (listToFract) . sequence . replicate 2 

isTrivial :: Fract -> Bool
isTrivial = undefined
