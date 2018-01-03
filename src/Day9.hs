module Day9 where

import Utils (test)

day9 :: String -> Int
day9 s = score s 0 0

day9extra :: String -> Int
day9extra = countGarbage 0

score :: String -> Int -> Int -> Int
score (',':cs) depth res = score cs depth res
score ('!':_:cs) depth res = score cs depth res
score ('{':cs) depth res = score cs (depth + 1) res
score ('}':cs) depth res = score cs (depth - 1) (res + depth)
score ('<':cs) depth res = score (snd $ garbage 0 cs) depth res
score [] 0 res = res
score cs depth res = error $ "unexpected state: depth=" ++ show depth ++ ", res=" ++ show res ++ ", cs=" ++ cs

countGarbage :: Int -> String -> Int
countGarbage n ('!':_:cs) = countGarbage n cs
countGarbage n ('<':cs) = let (k, cs2) = garbage 0 cs in countGarbage (n + k) cs2
countGarbage n (_:cs) = countGarbage n cs
countGarbage n [] = n

garbage :: Int -- ^ current garbage size
        -> String -- ^ left input
        -> (Int, String) -- ^ garbage size and left input
garbage n [] = (n, [])
garbage n ('!':_:cs) = garbage n cs
garbage n ('>':cs) = (n, cs)
garbage n (_:cs) = garbage (n + 1) cs

day9Test :: Bool
day9Test = all (test day9)
    [ ("{}", 1)
    , ("{{{}}}", 6)
    , ("{{},{}}", 5)
    , ("{{{},{},{{}}}}", 16)
    , ("{<a>,<a>,<a>,<a>}", 1)
    , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
    , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
    , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
    , ("{<>}", 1)
    , ("<random characters>", 0)
    , ("<<<<>", 0)
    , ("<{!>}>", 0)
    , ("<!!>", 0)
    , ("<!!!>>", 0)
    , ("<{o\"i!a,<{i<a>", 0)
    ]

day9extraTest :: Bool
day9extraTest = all (test day9extra)
    [ ("{}", 0)
    , ("{<a>,<a>,<a>,<a>}", 4)
    , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 8)
    , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 0)
    , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 17)
    , ("{<>}", 0)
    , ("<random characters>", 17)
    , ("<<<<>", 3)
    , ("<{!>}>", 2)
    , ("<!!>", 0)
    , ("<!!!>>", 0)
    , ("<{o\"i!a,<{i<a>", 10)
    ]