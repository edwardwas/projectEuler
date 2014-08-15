import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad (liftM)

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

minLimit :: Integral a => a
minLimit = intSqrt 1020304050607080900

maxLimit :: Integral a => a
maxLimit = intSqrt 1929394959697989990

toTest :: Integral a => [a]
toTest = 0 : (reverse [1..9])

takeAlt :: [a] -> [a]
takeAlt [] = []
takeAlt (x:[]) = [x]
takeAlt (x:_:xs) = x : takeAlt xs

digSum :: Integral a => a -> [a]
digSum = map (flip mod 10) . takeWhile (>0) . iterate (flip div 10)

testing :: Integral a => a -> Bool
testing = (==) toTest . takeAlt . digSum . (^2)

run :: IO [Integer]
run = CL.sourceList [minLimit .. maxLimit + 1] $$ CL.filter ((== 0) . flip mod 10) =$ 
		CL.filter (testing) =$ CL.consume 

main :: IO ()
main = run >>= return . head >>=  print
