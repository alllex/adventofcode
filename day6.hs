
import Data.List
import Data.Maybe

-- DAY 6

main :: IO ()
main = do
    let result = "hi"
    -- result <- day6 <$> readFile "day6.in"
    -- result <- day6extra <$> readFile "day6.in"
    -- let result = day6Tests
    -- let result = day6ExtraTests
    print result

-- DAY 6 PROBLEM

day6Tests :: Bool
day6Tests = let f = day6 in 
    (f "0 2 7 0" == 5)

day6 :: String -> Int
day6 = (\l -> l-1) . length . historyTillRepeat . map read . words

-- DAY 6 EXTRA PROBLEM

day6extraTests :: Bool
day6extraTests = let f = day6extra in 
    (f "0 2 7 0" == 4)

day6extra :: String -> Int
day6extra input = 1 + fromJust (elemIndex h hs)
    where
        (h:hs) = historyTillRepeat . map read . words $ input

historyTillRepeat :: [Int] -> [[Int]]
historyTillRepeat = sub []
    where
        sub history xs | xs `elem` history = xs:history
                       | otherwise = sub (xs:history) $ redist xs

redist :: [Int] -> [Int]
redist xs = map (\(i, x) -> x + inc i) indexed
    where 
        len = length xs
        indexed = zip [0..] xs
        (imx, mx) = foldl1 (\a@(_, m) (j, x) -> if x > m then (j, x) else a) indexed
        commonInc = mx `div` len
        extra = mx `mod` len
        startExtraIx = (imx + 1) `mod` len
        endExtraIx = (startExtraIx + extra) `mod` len
        hasExtra = if endExtraIx >= startExtraIx 
                   then \ix -> startExtraIx <= ix && ix < endExtraIx
                   else \ix -> startExtraIx <= ix || ix < endExtraIx
        inc ix = commonInc + (if hasExtra ix then 1 else 0) - (if ix == imx then mx else 0) 
