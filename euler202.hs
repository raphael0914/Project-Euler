main = print . (*2) . length . filter (\x -> (isValid x) && (not $ hasPrev x)) $ [ (6008819575, 6008819575-c, c) | c <- [1..3004409787], mod (6008819575-(2*c)) 3 == 0 ]

isValid :: (Int, Int, Int) -> Bool
isValid (a, b, c) = (a == b+c) && (mod (abs (b-c)) 3 ==0)

hasPrev :: (Int, Int, Int) -> Bool
hasPrev (a, b, c) = gcd c (gcd a b) /= 1