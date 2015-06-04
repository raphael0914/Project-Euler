import Data.Numbers.Primes

isGoldbach :: Int -> Bool
isGoldbach n = any isPrime . map ((-) n) . takeWhile (<n) $ [ 2*x*x | x <- [1..] ]

main = print . head . dropWhile (isGoldbach) $ [x*y | x <- [3, 5..191], y <- [3, 5..191]]