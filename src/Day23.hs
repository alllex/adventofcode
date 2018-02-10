module Day23 where

import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import qualified Data.StringMap as M
import qualified Data.Vector as V

import Utils (getOrPut)

type StringIntMap = M.StringMap Int

day23 :: String -> Int
day23 s = execPgm 0 upd M.empty $ toCmds s
    where
        upd acc _ cmd = case cmd of Mth "mul" _ _ _ -> acc + 1; _ -> acc

day23extra :: Int
day23extra = let n = 108400 in length [ x | x <- [n, n + 17 .. n + 17000], not $ isPrime x]

isPrime :: Int -> Bool
isPrime k = null [ x | x <- [2..1 + isqrt k], k `mod`x  == 0]

isqrt :: Int -> Int
isqrt = round . (sqrt :: Double -> Double) . fromIntegral

execPgm :: a -> (a -> StringIntMap -> Cmd -> a) -> StringIntMap -> [Cmd] -> a
execPgm start upd startMap cmds = run start (startMap, 0)
    where
        cs = V.fromList cmds
        cl = V.length cs
        isOut k = k < 0 || cl <= k
        run acc (rMap, i) 
            | isOut i = acc
            | otherwise = c `seq` next `seq` acc' `seq` run acc' next
                where
                    c = cs V.! i
                    next@(rMap', _) = execCmd (rMap, i) c 
                    acc' = upd acc rMap' c


toCmds :: String -> [Cmd]
toCmds = map (toCmd . splitOn " ") . lines

execCmd :: (StringIntMap, Int) -> Cmd -> (StringIntMap, Int)
execCmd (rMap, i) cmd = case cmd of
    (Mth _ r p op) -> (regOp rMap r p op, i + 1)
    (Jgz p1 p2) -> let (rMap', j) = doJump rMap i p1 p2 in (rMap', j)

doJump :: StringIntMap -> Int -> CmdParam -> CmdParam -> (StringIntMap, Int)
doJump rMap i p1 p2 = 
    let (p1Val, rMap1) = rMap `getParamValue` p1
        (p2Val, rMap2) = rMap1 `getParamValue` p2
        j = if p1Val /= 0 then i + p2Val else i + 1
    in (rMap2, j)

regOp :: StringIntMap -> String -> CmdParam -> (Int -> Int -> Int) -> StringIntMap
regOp rMap r p op = rMap3
    where
        (pVal, rMap1) = rMap `getParamValue` p
        (rVal, rMap2) = rMap1 `getParamValue` Reg r
        rMap3 = M.insert r (rVal `op` pVal) rMap2

getParamValue :: StringIntMap -> CmdParam -> (Int, StringIntMap)
getParamValue rMap (Val v) = (v, rMap)
getParamValue rMap (Reg r) = getOrPut rMap r 0

toCmd :: [String] -> Cmd
toCmd ["set", r, v] = Mth "set" r (toCmdParam v) (\_ u -> u)
toCmd ["sub", r, v] = Mth "sub" r (toCmdParam v) (-)
toCmd ["mul", r, v] = Mth "mul" r (toCmdParam v) (*)
toCmd ["jnz", x, y] = Jgz (toCmdParam x) (toCmdParam y)
toCmd vs = error $ "unexpected cmd: " ++ show vs

toCmdParam :: String -> CmdParam
toCmdParam s@(c:_)
    | isAlpha c = Reg s
    | otherwise = Val $ read s
toCmdParam s = error $ "unexpected cmd param: " ++ s

data Cmd 
    = Jgz CmdParam CmdParam
    | Mth String String CmdParam (Int -> Int -> Int)

instance Show Cmd where
    show (Jgz x y) = "Jgz (" ++ show x ++ ") (" ++ show y ++ ")" 
    show (Mth name r p _) = "Mth[" ++ name ++ "] " ++ r ++ " (" ++ show p ++ ")"

data CmdParam = Val Int | Reg String deriving Show