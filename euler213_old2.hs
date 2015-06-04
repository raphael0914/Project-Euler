import Data.Matrix
import Data.List

mFar :: Int -> Matrix Double
mFar n = zero n n


mEdgeOut :: Int -> Matrix Double
mEdgeOut n = setElem 6 (n, n) . setElem 6 (1, 1) 
             . fmap (*4) $ (identity n)


mInterOut :: Int -> Matrix Double
mInterOut n = setElem 4 (n, n) . setElem 4 (1, 1) 
              . fmap (*3) $ (identity n)

mCorner :: Int -> Matrix Double
mCorner n = setElem 6 (1, 2) . setElem 6 (n, n-1)
	     	. matrix n n 
	    	$ \(i,j) -> if ( abs (i-j) == 1) then 4 else 0

mDig :: Int -> Matrix Double
mDig n = setElem 4 (1, 2) . setElem 4 (n, n-1)
		. matrix n n 
		$ \(i,j) -> if ( abs (i-j) == 1) then 3 else 0



mTransfer :: Int -> Matrix Double
mTransfer n = fmap ( flip (/) (12) ) . foldl1 (<->) . map ( foldr1 (<|>) . map (chooseMx12 n) ) $ indexs
	where indexs = toLists . fromList n n $ [(x, y) | x <- [1..n], y <- [1..n]]


mTransfer10 :: Matrix Double
mTransfer10 = mTransfer 20

chooseMx12 :: Int -> (Int, Int) -> Matrix Double
chooseMx12 n (row, col)
	| (row == 1)   && ( col == 1)   = mCorner n
	| (row == n) && ( col == n)     = mCorner n
	| (row ==  col)                 = mDig n
	| (row == 1)   && ( col == 2)   = mEdgeOut n
	| (row == n) && ( col == (n-1)) = mEdgeOut n
	| (abs (row - col)) == 1        = mInterOut n
	| otherwise 					= mFar n



ringBell :: Int -> Matrix Double -> Matrix Double
ringBell n m = fromList dim dim . toList
               $ foldl' (*) (fromList 1 (dim*dim) . toList $ m) (take n $ repeat trans)
               where dim = (nrows m)
             	     trans = mTransfer10

ringBellEnd :: Matrix Double -> Matrix Double
ringBellEnd m = ringBell 50 m


ringBell' :: Matrix Double
ringBell' = foldl1' (*) (take 20 $ repeat mTransfer10)



initAll :: Int -> [Matrix Double]
initAll n = map (fromList n n) . toLists $ identity (n*n)



combine :: Matrix Double -> Matrix Double -> Matrix Double
combine m1 m2 = fromList dim dim $ zipWith (*) (toList m1) (toList m2)             
	where dim = (nrows m1)


--main = print . sum . toList . foldr1 (combine) 
--        . map ( (fmap ((-) 1)) . (flip (*)) ringBell' . fromList 1 100 . toList ) $ (initAll 10)


-- main = print . sum . toList $ ringBell'
main = print . sum . toList $ mTransfer10*mTransfer10


--main = print . foldr1 (+) . map (sum . toList) $ (initAll 30)


--mTransfer' :: Int -> Matrix Double
--mTransfer' n = fmap ((-) 1) $! mTransfer n


--mInit :: Int -> Matrix Double
--mInit n = matrix n n $ \(i, j) -> 1		


--countProb0 :: Matrix Double -> Matrix Double
--countProb0 m = fromList dim dim $ map ( ( (flip (/) (12^n)) ) . foldr1 (*) . zipWith (*) ms ) trans
--	where 
--		ms = toList m
--		dim = (nrows m)
--		n = (length ms)
--		trans = toLists . transpose $ mTransfer12' dim

--countExpect :: Matrix Double -> Double
--countExpect m = sum . toList $ countProb0 m

--ringBell :: Int -> Matrix Integer -> Matrix Integer
--ringBell n m = foldl (*) (fromList 1 (dim*dim) . toList $ m) (take n $ repeat trans)
--             where dim = (nrows m)
--             	   trans = mTransfer12 dim

--countProb0 :: Matrix Double -> Matrix Double
--countProb0 m = fromList dim dim $ map ( ( (flip (/) (12^n)) ) . foldr1 (*) . zipWith (*) ms ) trans
--	where 
--		ms = toList m
--		dim = (nrows m)
--		n = (length ms)
--		trans = toLists . transpose $ mTransfer12' dim             	   




--mTransfer4 :: Matrix Integer
--mTransfer4 = (mCorner   4 <|> mEdgeOut  4 <|> mFar      4   <|> mFar      4 ) <->
--			 (mInterOut 4 <|> mDig      4 <|> mInterOut 4   <|> mFar      4 ) <->
--			 (mFar      4 <|> mInterOut 4 <|> mDig      4   <|> mInterOut 4 ) <->
--			 (mFar      4 <|> mFar      4 <|> mEdgeOut  4   <|> mCorner   4 )