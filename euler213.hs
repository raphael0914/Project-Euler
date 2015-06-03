import Data.List


vInit :: Int -> [Double]
vInit n = 1.0:(take (n*n-1) $ repeat 0.0)

vInitAll :: Int -> [[Double]]
vInitAll n = [ (take (i-1) $ repeat 0.0) ++ [1.0] ++ (take (n*n-i) $ repeat 0.0) | i <- [1..(n*n)] ]

indexes :: Int -> [(Int, Int)]
indexes n = [(row, col) | row <- [1..n], col <- [1..n]]

d :: Int
d = 30

ringBell' :: [Double] -> [Double]
ringBell' squares = map (move squares) $ zip (indexes d) squares


ringBell :: Int -> [Double] -> [Double]
ringBell n m
	| n>0 = ringBell' $ ringBell (n-1) m
	| otherwise = id m


move :: [Double] -> ((Int, Int), Double) -> Double
move s ((r, c), _)
	| (r,c) == (1, 1) = (s!!(i2i (1, 2  )))/3 + (s!!(i2i (2  , 1)))/3
	| (r,c) == (1, d) = (s!!(i2i (1, d-1)))/3 + (s!!(i2i (2  , d)))/3
	| (r,c) == (d, 1) = (s!!(i2i (d, 2  )))/3 + (s!!(i2i (d-1, 1)))/3
	| (r,c) == (d, d) = (s!!(i2i (d, d-1)))/3 + (s!!(i2i (d-1, d)))/3
	| (r,c) == (1  , 2  ) = (s!!(i2i (1, 1)))/2 + (s!!(i2i (1  , 3  )))/3 + (s!!(i2i (2  , 2  )))/4
	| (r,c) == (2  , 1  ) = (s!!(i2i (1, 1)))/2 + (s!!(i2i (3  , 1  )))/3 + (s!!(i2i (2  , 2  )))/4
	| (r,c) == (1  , d-1) = (s!!(i2i (1, d)))/2 + (s!!(i2i (1  , d-2)))/3 + (s!!(i2i (2  , d-1)))/4
	| (r,c) == (2  , d  ) = (s!!(i2i (1, d)))/2 + (s!!(i2i (3  , d  )))/3 + (s!!(i2i (2  , d-1)))/4
	| (r,c) == (d  , 2  ) = (s!!(i2i (d, 1)))/2 + (s!!(i2i (d  , 3  )))/3 + (s!!(i2i (d-1, 2  )))/4
	| (r,c) == (d-1, 1  ) = (s!!(i2i (d, 1)))/2 + (s!!(i2i (d-2, 1  )))/3 + (s!!(i2i (d-1, 2  )))/4
	| (r,c) == (d  , d-1) = (s!!(i2i (d, d)))/2 + (s!!(i2i (d  , d-2)))/3 + (s!!(i2i (d-1, d-1)))/4
	| (r,c) == (d-1, d  ) = (s!!(i2i (d, d)))/2 + (s!!(i2i (d-2, d  )))/3 + (s!!(i2i (d-1, d-1)))/4
	| (r,c) == (2  , 2  ) = (s!!(i2i (1  , 2  )))/3 + (s!!(i2i (2  , 1  )))/3 + (s!!(i2i (2  , 3  )))/4 + (s!!(i2i (3  , 2  )))/4
	| (r,c) == (2  , d-1) = (s!!(i2i (1  , d-1)))/3 + (s!!(i2i (2  , d  )))/3 + (s!!(i2i (2  , d-2)))/4 + (s!!(i2i (3  , d-1)))/4
	| (r,c) == (d-1, 2  ) = (s!!(i2i (d-1, 1  )))/3 + (s!!(i2i (d  , 2  )))/3 + (s!!(i2i (d-1, 3  )))/4 + (s!!(i2i (d-2, 2  )))/4
	| (r,c) == (d-1, d-1) = (s!!(i2i (d-1, d  )))/3 + (s!!(i2i (d  , d-1)))/3 + (s!!(i2i (d-1, d-2)))/4 + (s!!(i2i (d-2, d-1)))/4
	| r == 1 = (s!!(i2i (1  , c-1)))/3 + (s!!(i2i (1  , c+1)))/3 + (s!!(i2i (2  , c  )))/4
	| r == d = (s!!(i2i (d  , c-1)))/3 + (s!!(i2i (d  , c+1)))/3 + (s!!(i2i (d-1, d  )))/4
	| c == 1 = (s!!(i2i (r-1, 1  )))/3 + (s!!(i2i (r+1, 1  )))/3 + (s!!(i2i (r  , 2  )))/4
	| c == d = (s!!(i2i (r-1, d  )))/3 + (s!!(i2i (r+1, d  )))/3 + (s!!(i2i (r  , d-1)))/4
	| r == 2    = (s!!(i2i (r+1, c)))/4 + (s!!(i2i (r-1, c)))/3 + (s!!(i2i (r, c+1)))/4 + (s!!(i2i (r, c-1)))/4
	| r == d-1  = (s!!(i2i (r+1, c)))/3 + (s!!(i2i (r-1, c)))/4 + (s!!(i2i (r, c+1)))/4 + (s!!(i2i (r, c-1)))/4
	| c == 2    = (s!!(i2i (r+1, c)))/4 + (s!!(i2i (r-1, c)))/4 + (s!!(i2i (r, c+1)))/4 + (s!!(i2i (r, c-1)))/3
	| c == d-1  = (s!!(i2i (r+1, c)))/4 + (s!!(i2i (r-1, c)))/4 + (s!!(i2i (r, c+1)))/3 + (s!!(i2i (r, c-1)))/4
	| otherwise = (s!!(i2i (r+1, c)))/4 + (s!!(i2i (r-1, c)))/4 + (s!!(i2i (r, c+1)))/4 + (s!!(i2i (r, c-1)))/4

i2i :: (Int, Int) -> Int
i2i (r, c) = d * (r-1) + (c-1)

combine :: [Double] -> [Double] -> [Double]
combine m1 m2 = zipWith (*) (map ((-) 1 ) m1)  (map ((-) 1 ) m2)


main = print . sum . foldl1' (zipWith (*)) . map ( map ((-) 1 ) . ringBell 50 ) $ vInitAll d


