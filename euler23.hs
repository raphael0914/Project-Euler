import Data.List

divisors :: Int -> [Int]
divisors n = filter  (\x -> rem n x == 0) $ [1..n]

isAbundant :: Int -> Bool
isAbundant n = ((sum . divisors $ n) - n ) > n

abundant :: [Int]
abundant = filter isAbundant $ [1..28123] -- 28123

isTwoAbundant :: Int -> Bool
isTwoAbundant n = any isAbundant . map ( (-) n ) $ takeWhile (< n) abundant



main = print . sum . filter (not . isTwoAbundant) $ [1..28123]


