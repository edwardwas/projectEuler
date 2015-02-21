{-# LANGUAGE ExistentialQuantification #-}


data Gaus = forall a . (Integral a, Eq a) => Gaus a a 

