module Day19 where

import qualified Data.Vector as V

import Utils (test)

day19 :: String -> String
day19 input = reverse . snd $ travel cells 0 (-1) startJ "" D
    where
        cells = V.fromList $ map (\l -> V.fromList $ map charToCell l) $ lines input
        startJ = findStart 0 $ V.toList $ V.head cells

day19extra :: String -> Int
day19extra input = fst $ travel cells 0 (-1) startJ "" D
    where
        cells = V.fromList $ map (\l -> V.fromList $ map charToCell l) $ lines input
        startJ = findStart 0 $ V.toList $ V.head cells

findStart :: Int -> [Cell] -> Int
findStart _ [] = error "start not found"
findStart i (c:cs)
    | c /= S = i
    | otherwise = findStart (i + 1) cs

travel :: V.Vector (V.Vector Cell) -> Int -> Int -> Int -> String -> Direction -> (Int, String)
travel cells steps ii jj path dir = 
    case cell of
        S -> (steps, path)
        (C c) -> travel cells (steps + 1) i j (c:path) dir
        X -> travel cells (steps + 1) i j path $ intersectDir cells i j dir
        _ -> travel cells (steps + 1) i j path dir
    where
        (i, j) = move ii jj dir
        cell = (cells V.! i) V.! j

intersectDir :: V.Vector (V.Vector Cell) -> Int -> Int -> Direction -> Direction
intersectDir cells i j dir = 
    if dir == U || dir == D 
    then if isNotEmpty cells i (j - 1)
         then L 
         else R
    else if isNotEmpty cells (i - 1) j
         then U
         else D

isNotEmpty :: V.Vector (V.Vector Cell) -> Int -> Int -> Bool
isNotEmpty cells i j = (cells V.! i) V.! j /= S

move :: Int -> Int -> Direction -> (Int, Int)
move i j dir =
    case dir of 
        U -> (i - 1, j)
        D -> (i + 1, j)
        L -> (i, j - 1)
        R -> (i, j + 1)

charToCell :: Char -> Cell
charToCell ' ' = S
charToCell '-' = H
charToCell '|' = V
charToCell '+' = X
charToCell c = C c

data Direction = U | D | L | R deriving (Show, Eq)
data Cell = H | V | X | S | C Char deriving (Eq)

instance Show Cell where
    show H = "-"
    show V = "|"
    show X = "+"
    show S = " "
    show (C c) = [c]

testDay19 :: Bool
testDay19 = all (test day19)
    [ ("     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ ", "ABCDEF")
    ]

testDay19extra :: Bool
testDay19extra = all (test day19extra)
    [ ("     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ ", 38)
    ]
