module Day22 where

import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Monad.ST
import Control.Monad
import Data.STRef

type NodeMap = V.Vector (V.Vector Int)

-- sampleInput = "..#\n#..\n..."

day22 input = nodesSeqAt input 1000 10000

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
        writeSTRef mSt (d', move ij d', if v' == 1 then c + 1 else c)
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

move (i, j) d = case d of
    U -> (i - 1, j)
    D -> (i + 1, j)
    L -> (i, j - 1)
    R -> (i, j + 1)

turnLeft U = L
turnLeft D = R
turnLeft L = D
turnLeft R = U

turnRight U = R
turnRight D = L
turnRight L = U
turnRight R = D

data Direction = U | D | L | R deriving (Show, Eq)