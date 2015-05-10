import Control.Monad
import Data.List
import System.Environment
import Data.Array

run :: Integer -> Integer -> Integer
run x n = arr ! n
    where arr = array (-n,n) $ zip [(-n)..n] $ map func [(-n)..n]
          func m = if x > m then 1 else 1 
                + sum [arr !  (m - s - bl - 1) | s <- [0..m-x], bl <- [x..m-s]]

fistPastMil x = head $ dropWhile (\n -> (10^6) >= run x n) [1..]

main = getArgs >>= print . fistPastMil . read . head


