module Day4 where 

import Data.List


day4 :: String -> Int
day4 = length . filter (not . containsDuplicates) . map words . lines

containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates [] = False
containsDuplicates (w:ws) = w `elem` ws || containsDuplicates ws

day4extra :: String -> Int
day4extra = length . filter (not . containsDuplicates . map sort) . map words . lines

isAnagramTo :: String -> String -> Bool
w1 `isAnagramTo` w2 = sort w1 == sort w2

day4Tests :: Bool
day4Tests = let f = day4 in 
    (f "a b a \n a b c \n a ab ba" == 2) &&
    (f "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa" == 2)
