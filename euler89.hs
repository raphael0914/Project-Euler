import Data.String.Utils

bestRoman :: String -> String
bestRoman str =  replace "CCCC" "CD" . replace "DCCCC" "CM" 
               . replace "XXXX" "XL" . replace "LXXXX" "XC"
               . replace "IIII" "IV" . replace "VIIII" "IX"
               $ str


main = do
  content <- readFile "p089_roman.txt"
  let romans = lines content
      counts = sum . map (length) $ romans
      saveCount = counts - ( sum . map (length . bestRoman) $ romans)
  print saveCount
