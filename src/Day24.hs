module Day24 where

import Data.List.Split (splitOn)

type Port = (Int, Int)

day24 :: String -> Int
day24 input = sub 0 0 ports
    where
        ports = readPorts input
        sub w e ps = 
            case extractFits e ps of
                [] -> w
                vs -> maximum [ sub (w + e' + ee) ee ps' | ((e', ee), ps') <- vs ]

day24extra :: String -> Int
day24extra input = snd $ sub 0 0 0 ports
    where
        ports = readPorts input
        sub :: Int -> Int -> Int -> [Port] -> (Int, Int)
        sub l w e ps = 
            case extractFits e ps of
                [] -> (l, w)
                vs -> maximum [ sub (l + 1) (w + e' + ee) ee ps' | ((e', ee), ps') <- vs ]


extractFits :: Int -> [Port] -> [(Port, [Port])]
extractFits e = sub []
    where
        sub _ [] = []
        sub left (p@(a, b):right) 
            | a == e = (p, rest) : others 
            | b == e = ((b, a), rest) : others 
            | otherwise = others
            where
                rest = left ++ right
                others = sub (p:left) right

readPorts :: String -> [(Int, Int)]
readPorts = map ((\[a, b] -> (read a, read b)) . splitOn "/") . lines