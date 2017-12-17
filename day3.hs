
import Data.Maybe
import Data.List
 ( find
 )

-- DAY 3

main :: IO ()
main = do
    -- result <- day3 . read <$> readFile "day3.in"
    -- result <- day3extra . read <$> readFile "day3.in"
    -- let result = day3Tests
    let result = "hi"
    print result

-- DAY 3 PROBLEM

{- Spiral
17  16  15  14  13  .
18   5   4   3  12  .
19   6   1   2  11  .
20   7   8   9  10 27
21  22  23  24  25 26
-}

day3Tests :: Bool
day3Tests = let f = day3 in
    (f 1 == 0) &&
    (f 12 == 3) &&
    (f 23 == 2) &&
    (f 1024 == 31)

day3 :: Int -> Int
day3 k = level - 1 + onLevelDiff
    where level = spiralLevel k
          side = level * 2 - 1
          area = side * side
          halfSide = (side - 1) `div` 2
          sideCenters = map ((area-) . (*halfSide)) [1, 3, 5, 7]
          onLevelDiff = minimum $ map (abs . (k-)) sideCenters

spiralLevelTests :: Bool
spiralLevelTests = let f = spiralLevel in
    (f 1 == 1) &&
    (f 2 == 2) && (f 7 == 2) && (f 9 == 2) &&
    (f 10 == 3) && (f 17 == 3) && (f 25 == 3) &&
    (f 26 == 4) && (f 30 == 4) && 
    (f 625 == 13)

spiralLevel :: Int -> Int
spiralLevel k = head [ l | l <- [1..], let s = l * 2 - 1, k <= s * s]

-- DAY 3 EXTRA PROBLEM

{- Spiral of sums
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
-}

day3extra :: Int -> Int
day3extra n = fromJust $ find (>n) spiralSums

indexedSpiralSums :: [(Int, Int)]
indexedSpiralSums = zip [1..] spiralSums

spiralSums :: [Int]
spiralSums = [1, 1, 2, 4, 5, 10, 11, 23, 25] ++ sub (drop 7 spiralSums) spiralSums (drop 9 spiral)
    where sub (p2:pp@(p1:_)) ss@(s3:ss2@(s2:s1:_)) ((_, marker):spiralTail) = curSum : sub pp nextSubs spiralTail
            where 
                nextSubs = if marker `elem` [Corner, PreCorner, Square] then ss else ss2
                curSum = case marker of
                            Corner -> p1 + s2
                            PostCorner -> p1 + p2 + s1 + s2
                            Normal -> p1 + s1 + s2 + s3
                            PreCorner -> p1 + s2 + s3
                            PreSquare -> p1 + s1 + s2 + s3
                            Square -> p1 + s2 + s3

data SpiralMarker = Normal | PreCorner | Corner | PostCorner | PreSquare | Square deriving (Show, Eq)

spiral :: [(Int, SpiralMarker)]
spiral = genSpiralLevels 0

genSpiralLevels :: Int -> [(Int, SpiralMarker)]
genSpiralLevels 0 = genSpiralLevels 1
genSpiralLevels 1 = (1, Normal) : genSpiralLevels 2
genSpiralLevels 2 = map (\n -> (n, Normal)) [2..9] ++ genSpiralLevels 3
genSpiralLevels n = zip [startId..] markers ++ genSpiralLevels (n + 1)
    where side = n * 2 - 1
          startId = (side - 2) ^ 2 + 1
          normalCount = side - 4
          normalMarkers = replicate normalCount Normal
          markers = Corner : PostCorner : drop 1 normalMarkers 
                     ++ (PreCorner : Corner : PostCorner : normalMarkers) 
                     ++ (PreCorner : Corner : PostCorner : normalMarkers) 
                     ++ (PreCorner : Corner : PostCorner : normalMarkers) 
                     ++ [PreSquare, Square]