import Data.Matrix

mFar :: Int -> Matrix Integer
mFar n = zero n n


mEdgeOut :: Int -> Matrix Integer
mEdgeOut n = setElem 6 (n, n) . setElem 6 (1, 1) 
             . fmap (*4) $ (identity n)


mInterOut :: Int -> Matrix Integer
mInterOut n = setElem 4 (n, n) . setElem 4 (1, 1) 
              . fmap (*3) $ (identity n)

mCorner :: Int -> Matrix Integer
mCorner n = setElem 6 (1, 2) . setElem 6 (n, n-1)
	     	. matrix n n 
	    	$ \(i,j) -> if ( abs (i-j) == 1) then 4 else 0

mID :: Int -> Matrix Integer
mID n = setElem 4 (1, 2) . setElem 4 (n, n-1)
		. matrix n n 
		$ \(i,j) -> if ( abs (i-j) == 1) then 3 else 0

mBig4 :: Matrix Integer
mBig4 = (mCorner 4   <|> mEdgeOut 4    <|> mFar 4        <|> mFar 4 ) <->
		(mInterOut 4 <|> mID 4         <|> mInterOut 4   <|> mFar 4 ) <->
		(mFar 4      <|> mInterOut 4   <|> mID 4         <|> mInterOut 4 ) <->
		(mFar 4      <|> mFar 4        <|> mEdgeOut 4    <|> mCorner 4 )

mBig4' :: Matrix Double
mBig4' = fmap (\x -> if (x/=0) then 1 - ( fromIntegral x / 12) else 0) mBig4

mInit :: Int -> Matrix Integer
mInit n = matrix n n $ \(i, j) -> 1


ringBell :: Int -> Matrix Integer -> Matrix Double
ringBell n m = fromList 4 4 . toList
	  		 . fmap ( flip (/) (fromIntegral (12^n)) . fromIntegral )
             $ foldl (*) (fromList 1 16 . toList $ mInit2 4) (take n $ repeat m)

--countExpect :: Matrix Double -> Double
--countExpect m = ((2/3)*m!(1, 2))^2*n + 
--                0.5*m!(1, 1)*(2/3)*m!(1, 2)*0.75*m!(2, 2)*(n-2)*4 +
--                ((2/3)*m!(1, 2))^2*((3/4)*m!(2, 3))^2*4
--                where n = 4


--countExpect :: Matrix Double -> Double
countExpect m = map (zipWith (flip (**)) mList) $ toLists mBig4'
                where n = 4
                      mList = toList m

countExpect2 = sum . map ( myProduct ) . countExpect . transpose $ (ringBell 51 mBig4)


myProduct :: [Double] -> Double
myProduct m = foldl lalala 1.0 m

lalala :: Double -> Double -> Double
lalala acc x = if (x/=0.0) then acc*x else acc

mInit2 :: Int -> Matrix Integer
mInit2 n = setElem 1 (1, 1) . matrix n n $ \(i, j) -> 0