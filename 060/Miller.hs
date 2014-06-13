
isPrime n k = undefined

getSD n = helper (n-1) 0
	where helper x i
		| even x = helper (div x 2) (i+1)
		| otherwise = (i,x)

expon a 0 _ = 1
expon a b e 
	| odd b = flip (mod) e $ (mod a e) * (mod (expon a (b-1) e) e)
	| otherwise = mod (p*p) e
	where p = flip (mod) e $ expon a (div b 2) e

innerLoop _ _ 0 = False
innerLoop n x c 
	| newX == 1 = False
	| newX == n - 1 = True
	| otherwise = innerLoop n newX (c-1)
	where newX = expon x 2 n
