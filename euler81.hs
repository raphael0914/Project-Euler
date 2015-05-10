import System.IO
import Data.Char
import Data.List
import Text.CSV

readInt :: String -> Int
readInt = read

minAcc :: Int -> (Int, Int) -> Int
minAcc prev (ac, x) = x + (min prev ac)

minAccRow :: [Int] -> [Int] -> [Int]
minAccRow acc xs = scanl minAcc (head acc + head xs) (tail $ zip acc xs)



--minAccRow' :: [Int] -> [Int] -> Int -> [Int]
--minAccRow' acc xs i
--	| i==0 = ((head xs): (minAccRow' acc xs (i+1)))
--	| i < (length xs) = ( (min (xs!!(i-1)) (acc!!i)): (minAccRow' acc xs (i+1)) )
--	| otherwise = []



euler81 :: [[Int]] -> Int
euler81 matrix = last . foldl1 minAccRow . extendM $ matrix

extendM :: [[Int]] -> [[Int]]
extendM m = [scanl1 (+) $ head m] ++ tail m


main = do
	content <- parseCSVFromFile "p081_matrix.txt"
	let 
		Right matrix' = content
		matrix = map (map readInt) matrix'
	putStrLn $ show (euler81 matrix)
	return ()