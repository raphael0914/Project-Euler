import Data.Numbers.Primes
import Debug.Trace


isCircularPrimes :: Int -> Bool
isCircularPrimes n = and . map ( isPrime . read) $ expandCircular (show n)
-- isCircularPrimes n = and . map ( isPrime . read) $ expandCircular (show . (trace $ show n) $ n)


expandCircular :: String -> [String]
expandCircular s = (trace s) . map (\x -> take n . drop x . cycle $ s) $ [0..(n-1)]
	where n = length s
	
main = print (length . filter isCircularPrimes . takeWhile (<1000000) $ primes)	