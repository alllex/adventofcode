
import Data.List()
import Data.Char

main :: IO ()
main = do
    -- result <- day1_2 <$> readFile "day1.in"
    -- result <- day1_1 <$> readFile "day1.in"
    -- let result = day1_1_tests 
    -- let result = day1_2_tests 
    let result = "hi"
    print result

-- PROBLEM DAY 1-1

day1_1_tests :: Bool
day1_1_tests = let f = day1_1 in 
    (f "1122" == 3) && 
    (f "1111" == 4) &&
    (f "1234" == 0) &&
    (f "91212129" == 9)

day1_1 :: String -> Int
day1_1 = sumOfFollowedBySame . map digitToInt . filter (`elem` ['0' .. '9'])

sumOfFollowedBySame :: (Eq t, Num t) => [t] -> t
sumOfFollowedBySame [] = 0
sumOfFollowedBySame [x] = x
sumOfFollowedBySame xs@(hd:_) = sub xs
    where sub [] = 0
          sub [y] = if y == hd then y else 0
          sub (y1:ys@(y2:_)) = (if y1 == y2 then y1 else 0) + sub ys

-- PROBLEM DAY 1-2

day1_2_tests :: Bool
day1_2_tests = let f = day1_2 in 
    (f "1212" == 6) && 
    (f "1221" == 0) &&
    (f "123425" == 4) &&
    (f "123123" == 12) &&
    (f "12131415" == 4)

day1_2 :: String -> Int
day1_2 input = 
    let digits = map digitToInt . filter (`elem` ['0' .. '9']) $ input in
    sum $ zipWith (\x y -> if x == y then x else 0) digits (shiftHalfway digits)

shiftHalfway :: [a] -> [a]
shiftHalfway xs = let (hd, tl) = splitAt (length xs `div` 2) xs in tl ++ hd

