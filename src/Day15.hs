module Day15 where

import Data.Char (isDigit)

day15 :: String -> Int
day15 s = length $ filter (uncurry (==)) $ take 40000000 $ modPairs (2 ^ 16) $ seqAB starterA starterB
    where
        [starterA, starterB] = filterIntsFromLines s

day15extra :: String -> Int
day15extra s = length $ filter (uncurry (==)) $ take 5000000 $ modPairs (2 ^ 16) $ pickySeqAB starterA starterB
    where
        [starterA, starterB] = filterIntsFromLines s

filterIntsFromLines :: String -> [Int]
filterIntsFromLines = map filterInt . lines
    where
        filterInt = (read :: (String -> Int)) . filter isDigit

modPairs :: Int -> [(Int, Int)] -> [(Int, Int)]
modPairs _ [] = []
modPairs m ((a, b):ps) = (a `mod` m, b `mod` m) : modPairs m ps

seqAB :: Int -> Int -> [(Int, Int)]
seqAB starterA starterB = zip (tail seqA) (tail seqB)
    where
        seqA = starterA : map (\ x -> (x * 16807) `mod` 2147483647) seqA
        seqB = starterB : map (\ x -> (x * 48271) `mod` 2147483647) seqB

pickySeqAB :: Int -> Int -> [(Int, Int)]
pickySeqAB starterA starterB = zip (filter (\x -> x `mod` 4 == 0) $ tail seqA) (filter (\x -> x `mod` 8 == 0) $ tail seqB)
    where
        seqA = starterA : map (\ x -> (x * 16807) `mod` 2147483647) seqA
        seqB = starterB : map (\ x -> (x * 48271) `mod` 2147483647) seqB

