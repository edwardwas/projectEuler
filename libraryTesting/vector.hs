import qualified Data.Vector as Vec

numDigits = (+) 1 . floor . logBase 10 . fromIntegral

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
