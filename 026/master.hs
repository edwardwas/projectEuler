
decList :: Integral a => a -> [a]
decList n = map (flip (mod) 10 . flip (div) n) $ iterate (*10) 10

slice :: [a] -> Int -> Int -> [a]
slice l s e = take (e-s) $ drop s l

testRepeat l period start  = (slice l s e) == (slice l (s+e) (e*2))
	where s = start
	      e = start + period


