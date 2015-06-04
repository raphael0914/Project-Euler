
import Data.Char

isHappy :: Int -> Bool
isHappy n = isHappy' n []


isHappy' :: Int -> [Int] -> Bool
isHappy' n mems
	| (next n) `elem` mems = False
	| (next n) == 1 = True
	| otherwise = isHappy' (next n) (n:mems)



next :: Int -> Int
next n = sum . map ((^2) . digitToInt) $ (show n)

main = print . length . filter (not . isHappy) $ [1..10000000]