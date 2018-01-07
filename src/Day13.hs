module Day13 where

import Data.List.Split (splitOn)

import Utils (test)


day13 :: String -> Int
day13 s = severity $ map ((\ [d, r] -> (read d :: Int, read r :: Int)) . splitOn ": ") $ lines s

day13extra :: String -> Int
day13extra s = head $ filter (not . (`isCaught` drs)) [0..]
    where
        drs = map ((\ [d, r] -> (read d, read r)) . splitOn ": ") $ lines s

isCaught :: Int -> [(Int, Int)] -> Bool
isCaught delay = any (\(d, r) -> ((d + delay) `mod` ((r - 1) * 2)) == 0)

severity :: [(Int, Int)] -> Int
severity = foldl sub 0
    where
        sub acc (d, r) = if d `mod` ((r - 1) * 2) == 0 then acc + d * r else acc

testDay13 :: Bool
testDay13 = all (test day13) 
    [ ("0: 3\n1: 2\n4: 4\n6: 4", 24)
    ]

testDay13extra :: Bool
testDay13extra = all (test day13extra) 
    [ ("0: 3\n1: 2\n4: 4\n6: 4", 10)
    ]