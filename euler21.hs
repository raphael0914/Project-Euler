
divisors :: Int -> [Int]
divisors n = filter (\d -> rem n d == 0) [1..n]

amicableNum :: Int -> Int
amicableNum n = sum . init $ divisors n

isAmicableNum :: Int -> Bool
isAmicableNum n = n == (amicableNum . amicableNum $ n) && n /= amicableNum n

euler21 = sum . filter (<10000) $ filter isAmicableNum [2..10000]

main = print euler21


