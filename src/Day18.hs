module Day18 where

import Data.List (foldl')
import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import qualified Data.StringMap as M

import Utils (getOrPut, test)

type StringIntMap = M.StringMap Int

-- day18 :: String -> Int
day18 s = recover (M.empty, Init, 0)
    where
        cmds = map (toCmd . splitOn " ") $ lines s
        cmdCount = length cmds
        isOut k = k < 0 || cmdCount <= k
        recover acc@(_, _, i) 
            | isOut i = error "index is out of cmd bounds"
            | otherwise = let cmd = cmds !! i 
                              acc'@(_, state', _) = execCmd acc cmd
                          in case state' of
                              (Recovered r) -> r
                              _ -> recover acc'

execCmd :: (StringIntMap, State, Int) -> Cmd -> (StringIntMap, State, Int)
execCmd (rMap, state, i) cmd = case cmd of
    (Play p) -> let (pVal, rMap') = rMap `getParamValue` p in (rMap', Played pVal, i + 1)
    (Set r p) -> runRegOp r p (\_ pv -> pv)
    (Add r p) -> runRegOp r p (+)
    (Mul r p) -> runRegOp r p (*)
    (Mod r p) -> runRegOp r p mod
    (Recover p) -> let (pVal, rMap') = rMap `getParamValue` p 
                       (Played lastPlayed) = state
                       state' = if pVal /= 0 then Recovered lastPlayed else state
                   in (rMap', state', i + 1)
    (Jump p1 p2) -> let (p1Val, rMap1) = rMap `getParamValue` p1
                        (p2Val, rMap2) = rMap1 `getParamValue` p2
                        j = if p1Val > 0 then i + p2Val else i + 1
                    in (rMap2, state, j)
    where
        runRegOp r p op = (regOp rMap r p op, state, i + 1)

data State = Init | Played Int | Recovered Int

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
toCmd ["snd", v] = Play $ toCmdParam v
toCmd ["set", r, v] = Set r $ toCmdParam v
toCmd ["add", r, v] = Add r $ toCmdParam v
toCmd ["mul", r, v] = Mul r $ toCmdParam v
toCmd ["mod", r, v] = Mod r $ toCmdParam v
toCmd ["rcv", v] = Recover $ toCmdParam v
toCmd ["jgz", x, y] = Jump (toCmdParam x) (toCmdParam y)
toCmd vs = error $ "unexpected cmd: " ++ show vs

toCmdParam :: String -> CmdParam
toCmdParam s@(c:_)
    | isAlpha c = Reg s
    | otherwise = Val $ read s
toCmdParam s = error $ "unexpected cmd param: " ++ s

data Cmd = Play CmdParam
         | Set String CmdParam
         | Add String CmdParam
         | Mul String CmdParam
         | Mod String CmdParam
         | Recover CmdParam
         | Jump CmdParam CmdParam
         deriving Show

data CmdParam = Val Int | Reg String deriving Show


testDay18 :: Bool
testDay18 = all (test day18) 
    [ ("set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2", 4)
    ]