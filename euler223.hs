
candidates n = [(a, b, c) | c <- [(n `div` 3) .. (n `div`2)], b <- [2..c], a <- [2..b]]

main = print . length . filter (\(a, b, c) -> (a+b>c) && (a+c>b) && (b+c>a) && (a^2+b^2==c^2+1))
	   $ candidates 2400