import Data.List

concatenatedProd :: Int -> String
concatenatedProd n = last . takeWhile ((<987654321) . read) 
					 . scanl1 (++) 
					 . map ( show . (*n) ) 
					 $ [1..]

isPandigital :: String -> Bool
isPandigital s = (length s == 9) && ( (length . nub $ s) == 9 ) && (not ('0' `elem` s))

readInt :: String -> Int
readInt i = read i

main = print . sort 
	   . map readInt
	   . filter isPandigital
	   . map concatenatedProd $ [1..9999]
