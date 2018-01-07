module Day14 where

import Data.List.Split (chunksOf)
import Numeric (readHex, showIntAtBase)
import Data.Char (intToDigit)
import Text.Printf (printf)
import qualified Data.Vector as V

import Day10 (knotHash)
import Utils (test)


day14 :: String -> Int
day14 s = sum $ map countHashBits hashes
    where
        hashes = map (knotHash . (\n -> s ++ "-" ++ show n)) [0..127]

day14extra :: String -> [String]
day14extra s = map (map (\ c -> if c == '0' then ' ' else '#') . hashBits) hashes
    where
        hashes = map (knotHash . (\n -> s ++ "-" ++ show n)) [0..127]

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