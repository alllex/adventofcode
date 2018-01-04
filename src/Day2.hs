module Day2 where

import Data.Maybe

import Utils (minmax, strToTable, orElse, test)

day2 :: String -> Int
day2 = sum . map (\row -> let (mn, mx) = minmax row in mx - mn) . strToTable 

day2extraTest :: Bool
day2extraTest = let f = day2extra in 
    (f "5 9 2 8\n 9 4 7 3\n 3 8 6 5" == 9)

day2extra :: String -> Int
day2extra = sum . map (fromMaybe 0 . findDivdiv) . strToTable

-- Finds a result of the first divdiv in the list
findDivdiv :: [Int] -> Maybe Int
findDivdiv [] = Nothing
findDivdiv [_] = Nothing
findDivdiv (x:xs@(y:ys)) = divdiv x y `orElse` findDivdiv (x:ys) `orElse` findDivdiv xs

-- Returns division result when either argument divides the other.
divdiv :: Int -> Int -> Maybe Int
divdiv x y | x `mod` y == 0 = Just $ x `div` y
           | y `mod` x == 0 = Just $ y `div` x
           | otherwise      = Nothing

day2Test :: Bool
day2Test = all (test day2)
    [ ("5 1 9 5 \n 7 5 3 \n 2 4 6 8", 18)
    ]