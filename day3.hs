
import Data.List()

-- DAY 3

main :: IO ()
main = do
    let result = day3 312051
    -- let result = day3_1_tests 
    -- let result = day3Tests
    -- let result = "hi"
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