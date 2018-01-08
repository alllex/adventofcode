module Day14 where

import Data.List.Split (chunksOf)
import Numeric (readHex)
import Text.Printf (printf)

import Day10 (knotHash)


day14 :: String -> Int
day14 s = sum $ map countHashBits hashes
    where
        hashes = map (knotHash . (\n -> s ++ "-" ++ show n)) [0..127]

day14extra :: String -> Int
day14extra s = let (finalColor, _) = bfsColorGrid 128 bitHashes in finalColor - 1
    where
        bitHashes = map (map (\c -> if c == '0' then 0 else -1) . hashBits) hashes
        hashes = map (knotHash . (\n -> s ++ "-" ++ show n)) [0..127]

bfsColorGrid :: Int -> [[Int]] -> (Int, [[Int]])
bfsColorGrid size grid = foldl sub (1, grid) indices
    where
        indices = [(i, j) | i <- [0..size - 1], j <- [0..size - 1]]
        checkIndices (i, j) = 0 <= i && i < size && 0 <= j && j < size
        sub acc@(color, colorGrid) p 
            | colorGrid `get2d` p < 0 = (color + 1, bfs [p] checkIndices colorGrid color)
            | otherwise = acc

bfs :: [(Int, Int)] -- ^ queue of indices
    -> ((Int, Int) -> Bool) -- ^ valid indices checker
    -> [[Int]] -- ^ color grid
    -> Int -- ^ current color
    -> [[Int]] -- ^ new grid
bfs [] _ colorGrid _ = colorGrid
bfs (c@(i, j):queue) checkIJ colorGrid color = bfs (queue ++ next) checkIJ newGrid color
    where
        isUncolored p = checkIJ p && colorGrid `get2d` p < 0
        neigbours = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
        next = filter (\p -> p `notElem` queue && isUncolored p) neigbours
        newGrid = set2d colorGrid c color
    
get2d :: [[a]] -> (Int, Int) -> a
xxs `get2d` (i, j) = (xxs !! i) !! j

-- | Very inefficient implementation of 2d array
set2d :: [[a]] -> (Int, Int) -> a -> [[a]]
set2d xxs (i, j) v = map (\ (k, xs) -> if k == i then sub $ zip [0..] xs else xs) $ zip [0..] xxs
    where
        sub = map (\ (k, y) -> if k == j then v else y)

countHashBits :: String -> Int
countHashBits = sum . map (countBits 8 . hexStr2int) . chunksOf 2

hashBits :: String -> String
hashBits = concatMap (int2bitStr 8 . hexStr2int) . chunksOf 2

hexStr2int :: String -> Int
hexStr2int = fst . head . readHex

countBits :: Int -> Int -> Int
countBits len = sum . map (\c -> if c == '1' then 1 else 0) . int2bitStr len

int2bitStr :: Int -> Int -> String
int2bitStr len = printf ("%0" ++ show len ++ "b")