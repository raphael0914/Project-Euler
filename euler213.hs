
indexes :: Int -> [(Int, Int)]
indexes n = [(x, y) | x <- [1..n], y <- [1..n]]

vInit :: Int -> [Double]
vInit n = 1.0:(take (n-1) $ repeat 0.0)

dim :: Int
dim = 3

ringBell :: [Double] -> [((Int, Int), Double)]	
ringBell vec = map (move vec) $ zip (indexes dim) vec


move :: [Int] -> (Int, Int) -> Double
move v0 (row, col) = 0.5