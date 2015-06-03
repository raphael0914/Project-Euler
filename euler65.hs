import Data.Ratio
import Data.Char


xs :: [Integer]
xs = (2: (take 99 . concat $ map (\k -> (1:2*k:1:[])) [1..]) )


main = print . sum . map digitToInt . show . numerator . foldr (\x acc -> x%1 + 1/acc) ( (last xs) % 1 ) $ init xs
