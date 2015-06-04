
fibs = (1:1:zipWith (+) (tail fibs) fibs)
main = print . head . dropWhile (\(i, fib) -> (length . show $ fib) < 1000) $ zip [1..] fibs