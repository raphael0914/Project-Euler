import Data.Numbers.Primes
import Data.List
import Data.Ord

formula :: (Int, Int) -> Int -> Int
formula (a, b) n = n * n + a * n + b


nprimes :: (Int -> Int) -> Int
nprimes f = length . takeWhile (isPrime) . map (f) $ [0..]


euler27 :: (Int, (Int, Int))
euler27 = 
	let 
		cs = [ (a, b) | a <- [-1000..1000], b <- [-1000..1000] ]
		nps = map nprimes . map formula $ cs
	in 
		maximumBy (comparing fst) $ zip nps cs

main = print euler27


