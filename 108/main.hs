import Data.Numbers.Primes
import Data.List (nub)
import System.Environment (getArgs)
import Control.Applicative

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)


factorize :: Integral a => a -> [a]
factorize n
	| null f = [n]
	| otherwise = (head f):(factorize $ div n $ head f)
	where f = filter ((== 0) . mod n) $ takeWhile (<= (intSqrt n))  primes

numAnswers :: Integral a => a -> Int
numAnswers n = flip div 2 $ (+1) $ product $ map ((+1) . (*2) .  flip count fs) $ nub fs
	where fs = factorize n

lotsOPrimes :: Integral a => a -> a
lotsOPrimes n  = product $ takeWhile (<n) primes

run :: Integral a => Int -> a -> a
run t n = head $ filter ((>t) . numAnswers) $ map (*lotsOPrimes n) [1..]

main :: IO ()
main = (run <$> (read . head <$> getArgs) <*> (read . head . tail <$> getArgs)) >>= print

