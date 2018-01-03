module Day11 where

import Data.List.Split

import Utils (test)

day11 :: String -> Int
day11 = dist . foldl upd (0, 0) . splitOn ","

day11extra :: String -> Int
day11extra = fst . foldl sub (0, (0, 0)) . splitOn ","
    where
        sub (maxDist, coords) dir = let newCoords = upd coords dir in (maxDist `max` dist newCoords, newCoords)

upd :: (Int, Int) -> String -> (Int, Int)
upd (v, h) dir = case dir of 
    "ne" -> (v + 1, h + 1)
    "nw" -> (v + 1, h - 1)
    "n"  -> (v + 2, h)
    "se" -> (v - 1, h + 1)
    "sw" -> (v - 1, h - 1)
    "s"  -> (v - 2, h)
    _    -> error $ "unexpected direction: " ++ dir

dist :: (Int, Int) -> Int
dist (vv, hh) = if v <= h then h else h + (v - h) `div` 2
    where v = abs vv
          h = abs hh

day11Test :: Bool
day11Test = all (test day11)
    [ ("se,sw", 1)
    , ("ne,se", 2)
    , ("n,se,s,sw,nw,n,ne", 1)
    , ("n,ne", 2)
    , ("ne,ne,ne", 3)
    , ("ne,ne,sw,sw", 0)
    , ("ne,ne,s,s", 2)
    , ("se,sw,se,sw,sw", 3)
    , ("nw,se", 0)
    , ("n", 1)
    ]

day11extraTest :: Bool
day11extraTest = all (test day11extra)
    [ ("se,sw", 1)
    , ("ne,se", 2)
    , ("n,se,s,sw,nw,n,ne", 1)
    , ("n,ne", 2)
    , ("ne,ne,ne", 3)
    , ("ne,ne,sw,sw", 2)
    , ("ne,ne,s,s", 2)
    , ("se,sw,se,sw,sw", 3)
    , ("nw,se", 1)
    , ("n", 1)
    ]