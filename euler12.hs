import Data.Numbers.Primes
import Data.List
import Debug.Trace

primesCandidates :: Int -> [Int]
primesCandidates n = takeWhile ( <= (floor . sqrt . fromIntegral $ n) ) primes


primesFactors' :: Int -> Int -> [Int]
primesFactors' n i
	| i >= len && n/=0 = [n]
	| i >= len = []
	| rem n den == 0 = (den : primesFactors' (div n den) 0)
	| otherwise = primesFactors' n (i+1)
	where 
		pcs = primesCandidates n
		den = pcs!!i
		len = length pcs


primesFactors :: Int -> [Int]
primesFactors n = primesFactors' n 0


triangularNDivisors :: Int -> Int
triangularNDivisors n = foldr ((*) . (+1) . length) 1  
	. group . tail . sort 
	$ (primesFactors n ++ primesFactors (n+1))


euler12 :: Int -> Int
euler12 = ( snd n + 1) * (snd n + 2) `div` 2 
	where n = head . dropWhile ( (<1000) . fst) $ zip (map (triangularNDivisors) [2..]) [1..]

main = print euler12