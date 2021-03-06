module Day21 where

import Data.List.Split (splitOn, chunksOf)
import Data.List (group, sort, transpose)
import Data.Maybe (fromJust)
import Control.Arrow (first)

type Grid = [[Int]]

day21 :: String -> Int
day21 input = countActivePixels $ gridSeq input !! 5

day21extra :: String -> Int
day21extra input = countActivePixels $ gridSeq input !! 18

countActivePixels :: Grid -> Int
countActivePixels = sum . map sum

gridSeq :: String -> [Grid]
gridSeq rulesStr = iterate growGrid startGrid
    where
        startGrid :: Grid
        startGrid = [[0, 1, 0], [0, 0, 1], [1, 1, 1]]

        ruleBook :: [([Grid], Grid)] 
        ruleBook = map (first transformations) $ readRules rulesStr

        enhance :: Grid -> Grid 
        enhance g = fromJust $ head $ filter (/= Nothing) $ map (\(ins, outs) -> if g `elem` ins then Just outs else Nothing) ruleBook

        growGrid :: Grid -> Grid
        growGrid = mergeGrid . map (map enhance) . splitGrid

splitGrid :: Grid -> [[Grid]]
splitGrid g = if length g `mod` 2 == 0 then sp 2 g else sp 3 g
    where
        sp n = map (transpose . map (chunksOf n)) . chunksOf n

mergeGrid :: [[Grid]] -> Grid
mergeGrid = concatMap (map concat . transpose)

readRules :: String -> [([[Int]], [[Int]])]
readRules = map ((\[f, t] -> (readGrid f, readGrid t)) . splitOn " => ") . lines

readGrid :: String -> [[Int]]
readGrid = map (map (\c -> if c == '#' then 1 else 0)) . splitOn "/"

transformations :: Ord a => [[a]] -> [[[a]]]
transformations grid = rmdups $ let rs = rotates grid in rs ++ map flipGrid rs

rotates :: [[a]] -> [[[a]]]
rotates = take 4 . iterate rotateGrid

flipGrid :: [a] -> [a]
flipGrid = reverse

rotateGrid :: [[a]] -> [[a]]
rotateGrid = reverse . transpose
        
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
