module Day1 where

import Data.Char

import Utils (test)


day1 :: String -> Int
day1 = sumOfFollowedBySame . map digitToInt . filter (`elem` ['0' .. '9'])

day1extra :: String -> Int
day1extra input = 
    let digits = map digitToInt . filter (`elem` ['0' .. '9']) $ input in
    sum $ zipWith (\x y -> if x == y then x else 0) digits (shiftHalfway digits)

sumOfFollowedBySame :: (Eq t, Num t) => [t] -> t
sumOfFollowedBySame [] = 0
sumOfFollowedBySame [x] = x
sumOfFollowedBySame xs@(hd:_) = sub xs
    where sub [] = 0
          sub [y] = if y == hd then y else 0
          sub (y1:ys@(y2:_)) = (if y1 == y2 then y1 else 0) + sub ys

shiftHalfway :: [a] -> [a]
shiftHalfway xs = let (hd, tl) = splitAt (length xs `div` 2) xs in tl ++ hd

day1Test :: Bool
day1Test = all (test day1)
    [ ("1122", 3)
    , ("1111", 4)
    , ("1234", 0)
    , ("91212129", 9)
    ]

day1extraTest :: Bool
day1extraTest = all (test day1extra)
    [ ("1212", 6)
    , ("1221", 0)
    , ("123425", 4)
    , ("123123", 12)
    , ("12131415", 4)
    ]