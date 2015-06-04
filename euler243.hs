import Debug.Trace
import Data.Numbers.Primes

---- > 387074
----resilience n | trace ("resilience " ++ show n) False = undefined
--resilience :: Fractional a => Int -> (Int, a)
--resilience n = (n, (fromIntegral len) / (fromIntegral (n-1) ))
--	where len = length . filter (\x -> gcd x n == 1) $ [1..(n-1)]

----main = print . head . dropWhile ( >= 0.1635881955585578 ) $ map resilience [387074..]
--main = print . head . dropWhile ( \(i, r) -> r >= 0.1635881955585578 ) $ map (mytrace . resilience) [387074..]

mytrace :: (Show t1, Show t, Integral t) => (t, t1) -> (t, t1)
mytrace (i, r)
	| i `rem` 100000 == 0 = trace (show (i, r )) (i, r)
	| otherwise = (i, r)




primesFactors :: Int -> [Int]
primesFactors n = filter (\p -> rem n p == 0) . takeWhile ( <= n') $ primes
	where n' = floor . sqrt . fromIntegral $ n

phi' :: Int -> Double -> Double
phi' p acc = acc * ( 1.0 - ( 1.0 /  ( fromIntegral p ) ) ) 

phi :: Int -> Int
phi n = round $ foldr phi' (fromIntegral n) (primesFactors n)

resilience :: Fractional a => Int -> (Int, a)
resilience n = (n, (fromIntegral . phi $ n) / (fromIntegral (n-1)))

main = print . head . dropWhile ( \(i, r) -> r >= 0.1635881955585578 ) $ map (mytrace . resilience) [2310, 4620..]