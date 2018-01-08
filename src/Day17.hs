module Day17 where

import Data.List (elemIndex, foldl')

day17 :: String -> Int
day17 s = xs !! ((i + 1) `mod` length xs)
    where 
        shift = read s :: Int
        sub (i, xs) n = let j = (i + shift) `mod` length xs in (j + 1, insertAfter xs (j + 1) n)
        (i, xs) = foldl' sub (0, [0]) [1..2017]

day17extra :: String -> Int
day17extra s = xs !! ((zIndex + 1) `mod` length xs)
    where 
        shift = read s :: Int
        sub (i, xs) n = let j = (i + shift) `mod` length xs in (j + 1, insertAfter xs (j + 1) n)
        (i, xs) = foldl' sub (0, [0]) [1..50 * 10 ^ 6]
        (Just zIndex) = 0 `elemIndex` xs

insertAfter :: [a] -> Int -> a -> [a]
insertAfter xs 0 a = a:xs
insertAfter (x:xs) n a = x : insertAfter xs (n - 1) a
insertAfter [] _ _ = error "index out of bounds"