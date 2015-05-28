import Text.CSV
import Data.Char
import Data.List

countNameScore :: String -> Int
countNameScore s = sum . map ( ( flip (-) 64 ) . ord) $ s



primes = 2 : [i | i <- [3..],  and [rem i p > 0 | p <- takeWhile ((<=i).(^2)) primes]]

main = do
	a <- parseCSVFromFile "p022_names.txt"
	let 
		Right c = a
		d = sort . head $ c
	print ( sum $ zipWith (*) (map countNameScore $ d) [1..] )