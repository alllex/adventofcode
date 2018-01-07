module Day10 where

import Data.List.Split (chunksOf, splitOn)
import Data.List (foldl')
import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)

import Utils (test)


day10 :: String -> Int
day10 s = v1 * v2
    where 
        (v1:v2:_) = knotRotate 256 . map read . splitOn "," $ s

day10extra :: String -> String
day10extra = knotHash

knotHash :: String -> String
knotHash = hexedHash . denseHash . sparseHash 256 . map ord

sparseHash :: Int -> [Int] -> [Int]
sparseHash size lengths = knotRotate size lengths64
    where
        lengths64 = concat . replicate 64 $ lengths ++ extraLengths

denseHash :: [Int] -> [Int]
denseHash xs = map (foldl1 xor) $ chunksOf 16 xs

hexedHash :: [Int] -> String
hexedHash = concatMap ((\s -> if length s == 1 then '0':s else s) . (`showHex` ""))

extraLengths :: [Int]
extraLengths = [17, 31, 73, 47, 23]

knotRotate :: Int -> [Int] -> [Int]
knotRotate size lengths = rotate rotated finalShift
    where
        list = [0..size - 1]
        indexedLengths = zip [0..] lengths
        shiftSum = sum $ map (uncurry (+)) indexedLengths
        finalShift = size - (shiftSum `mod` size)
        sub xs (skipSize, reverseLength) = knotIter size skipSize reverseLength xs
        rotated = foldl' sub list indexedLengths

knotIter :: Int -> Int -> Int -> [Int] -> [Int]
knotIter size skipSize reverseLength xs = rotated
    where 
        partiallyReversed = reversePart xs reverseLength
        rotateLength = (reverseLength + skipSize) `mod` size
        rotated = rotate partiallyReversed rotateLength


reversePart :: [a] -> Int -> [a]
reversePart xs n = let (start, end) = splitAt n xs in reverse start ++ end

rotate :: [a] -> Int -> [a]
rotate xs n = let (start, end) = splitAt n xs in end ++ start

testReversePart :: Bool
testReversePart = all (test (reversePart [1..5]))
    [ (0, [1..5])
    , (1, [1..5])
    , (2, [2, 1, 3, 4, 5])
    , (5, [5, 4, 3, 2, 1])
    ]

testRotate :: Bool
testRotate = all (test (rotate [1..5]))
    [ (0, [1..5])
    , (1, [2, 3, 4, 5, 1])
    , (2, [3, 4, 5, 1, 2])
    , (5, [1..5])
    ]

testDay10 :: Bool
testDay10 = all (test (knotRotate 5))
    [ ([3, 4, 1, 5], [])

    ]
