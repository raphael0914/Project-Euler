import Data.List


productLit :: Int -> Int -> String
productLit n m = (show (n*m)) ++ (show n) ++ (show m)

isPandigital :: String -> Bool
isPandigital s = (length s == 9) && ( (length . nub $ s) == 9 ) && (not ('0' `elem` s))

pan14 :: [Int]
pan14 = [ x*y | x <- [1..9], y <- [1000..9999], isPandigital (productLit x y)]

pan23 :: [Int]
pan23 = [ x*y | x <- [10..99], y <- [100..999], isPandigital (productLit x y)]