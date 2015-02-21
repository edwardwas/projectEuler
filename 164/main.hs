import System.Environment
import Data.Array

nextOptionp :: (Integral a, Ix a) => (a,a) -> [a]
nextOptionp = (!) arr  
	where arr = array ((0,0),(9,9)) [((a,b),nextOption'(a,b)) | a <- [0..9], b <- [0..9]]
	      nextOption' (a,b) = [0.. (9 - a - b)]

nextOption (a,b) = [0.. (9 - a - b)]

type StoreNum = (Integer,Integer) 

expandStoreNum :: StoreNum -> [StoreNum]
expandStoreNum x@(_,b) = map ((,) b) $ nextOption x

start :: [StoreNum]
start = map (\x -> (0,x)) [1..9]

run n = length $ last $ take n $ iterate (concat . map expandStoreNum) start

main = getArgs >>= print . run . read . head
