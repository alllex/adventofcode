module Day9 where

day9Test :: Int
day9Test = length $ map (test day9)
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

day9 :: String -> Int
day9 s = score s 0 0

score :: String -> Int -> Int -> Int
score (',':cs) depth res = score cs depth res
score ('!':_:cs) depth res = score cs depth res
score ('{':cs) depth res = score cs (depth + 1) res
score ('}':cs) depth res = score cs (depth - 1) (res + depth)
score ('<':cs) depth res = score (garbage cs) depth res
score [] 0 res = res
score cs depth res = error $ "unexpected state: depth=" ++ show depth ++ ", res=" ++ show res ++ ", cs=" ++ cs

garbage :: String -> String
garbage [] = []
garbage ('!':_:cs) = garbage cs
garbage ('>':cs) = cs
garbage (_:cs) = garbage cs

test :: (Eq b, Show a, Show b) => (a -> b) -> (a, b) -> ()
test f (arg, expected) = if actual == expected then () else error errMsg
    where
        actual = f arg
        errMsg = "For arg `" ++ show arg ++ "` expected result `" ++ show expected ++ "`, got `" ++ show actual
