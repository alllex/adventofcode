module Day22 where

import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Monad.ST
import Control.Monad
import Data.STRef

day22 :: String -> Int
day22 input = nodesSeqAt input 1000 10000

day22extra :: String -> Int
day22extra input = let (_, _, c) = nodesAt input 500 10000000 in c

nodesAt :: String -> Int -> Int -> (Direction, (Int, Int), Int)
nodesAt input n k = runST $ do 
    let vecMap = map V.fromList initMap
    mMap <- V.fromList <$> mapM V.thaw vecMap 
    mSt <- newSTRef (U, (start, start), 0)
    replicateM_ k $ do 
        (d, ij@(i, j), c) <- readSTRef mSt
        let row = mMap V.! i
        v <- M.read row j
        let v' = visitNode v
        let d' = changeDir v d
        let c' = if v' == I then c + 1 else c
        M.write row j v'
        writeSTRef mSt (d', move ij d', c')
    readSTRef mSt
    where
        start = (n + 1) `div` 2 - 1
        initMap = map (map (\x -> if x == 1 then I else C)) $ wrapMap n $ readMap input

changeDir :: NodeState -> Direction -> Direction
changeDir v d = case v of 
    I -> turnRight d
    C -> turnLeft d
    F -> turnBack d
    W -> d

visitNode :: NodeState -> NodeState
visitNode F = C
visitNode I = F
visitNode W = I
visitNode C = W

nodesSeqAt :: String -> Int -> Int -> Int
nodesSeqAt input n k = runST $ do 
    let vecMap = map V.fromList $ wrapMap n initMap
    mMap <- V.fromList <$> mapM V.thaw vecMap 
    mSt <- newSTRef (U, (start, start), 0)
    replicateM_ k $ do 
        (d, ij@(i, j), c) <- readSTRef mSt
        let row = mMap V.! i
        v <- M.read row j
        let v' = 1 - v
        let d' = if v == 1 then turnRight d else turnLeft d
        let c' = if v' == 1 then c + 1 else c
        M.write row j v'
        writeSTRef mSt (d', move ij d', c')
    (_, _, c) <- readSTRef mSt
    return c
    where
        start = (n + 1) `div` 2 - 1
        initMap = readMap input

readMap :: String -> [[Int]]
readMap = map (map (\c -> if c == '#' then 1 else 0)) . lines

wrapMap :: Int -> [[Int]] -> [[Int]]
wrapMap n mp = fullZerosLayer ++ map wrapLevel mp ++ fullZerosLayer
    where
        l = length mp
        emptyCount = (n - l) `div` 2
        fullZeros = replicate n 0
        suffixZeros = replicate emptyCount 0
        fullZerosLayer = replicate emptyCount fullZeros
        wrapLevel lvl = suffixZeros ++ lvl ++ suffixZeros

move :: (Int, Int) -> Direction -> (Int, Int)
move (i, j) d = case d of
    U -> (i - 1, j)
    D -> (i + 1, j)
    L -> (i, j - 1)
    R -> (i, j + 1)

turnLeft :: Direction -> Direction
turnLeft U = L
turnLeft D = R
turnLeft L = D
turnLeft R = U

turnRight :: Direction -> Direction
turnRight U = R
turnRight D = L
turnRight L = U
turnRight R = D

turnBack :: Direction -> Direction
turnBack U = D
turnBack D = U
turnBack L = R
turnBack R = L

data Direction = U | D | L | R deriving (Show, Eq)

data NodeState = F | W | I | C deriving (Show, Eq)