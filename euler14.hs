import Data.List
import Data.Ord

-- 0m4.208s
collatz :: [Int] -> Int -> [Int]
collatz acc n
	| n == 1 = (1:acc)
	| even n = collatz (n:acc) (div n 2) 
	| odd n = collatz (n:acc) (3 * n + 1)

-- 0m5.154s
collatz' :: Int -> [Int]
collatz' n
	| n == 1 = [1]
	| even n = (n : collatz' (div n 2) )
	| odd n = (n : collatz' (3 * n + 1) )

main = print . (maximumBy (comparing fst)) . zip (map (length . (collatz')) [1..]) $ [1..1000000]