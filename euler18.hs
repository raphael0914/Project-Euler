import System.IO
import Data.Char
import Data.List

readInt :: String -> Int
readInt = read

maxAcc :: [Int] -> [Int]
maxAcc acc = zipWith (max) (acc ++ [0]) (0:acc)
-- [[3],[7,4],[2,4,6],[8,5,9,3]]
-- [3], [7, 4] -> [10, 7]
-- [10, 7], [2, 4, 6] -> []
maxAccRow :: [Int] -> [Int] -> [Int]
maxAccRow acc xs = zipWith (+) (maxAcc acc) xs



euler18 :: [[Int]] -> Int
euler18 triangle = maximum $ foldl1 maxAccRow triangle




main = do
	handle <- openFile "p067_triangle.txt" ReadMode
	contents <- hGetContents handle
	let triangle = map (map readInt . words) $ lines contents
	putStrLn $ show (euler18 triangle)
	return ()