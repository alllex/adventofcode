module Day18 where

import Data.List (foldl')
import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import qualified Data.StringMap as M

import System.IO.Unsafe (unsafePerformIO)

import Utils (getOrPut, test)

type StringIntMap = M.StringMap Int

day18 :: String -> Int
day18 s = recover (M.empty, Init, 0)
    where
        cmds = toCmds s
        cmdCount = length cmds
        isOut k = k < 0 || cmdCount <= k
        recover acc@(_, _, i) 
            | isOut i = error "index is out of cmd bounds"
            | otherwise = let cmd = cmds !! i 
                              acc'@(_, state', _) = execSoundCmd acc cmd
                          in case state' of
                              (LastRcv r) -> r
                              _ -> recover acc'

day18extra :: String -> Int
day18extra s = runBoth 0 (Norm, (rMapInit0, 0, []), []) (Norm, (rMapInit1, 0, []), [])
    where
        rMapInit0 = M.singleton "p" 0
        rMapInit1 = M.singleton "p" 1
        commands = toCmds s
        cmdCount = length commands
        isOut k = k < 0 || cmdCount <= k

        hangs (Term, _, _) = True
        hangs (Wait, _, _) = True
        hangs _ = False

        runBoth sentCount1 state1 state2@(st2, (rMap2, index2, incoming2), outgoing2)
            | hangs state1 && hangs state2 = sentCount1
            | otherwise = 
                let (st1', (rMap1', index1', incoming1'), outgoing1') = run state1
                    (st2', acc2', outgoing2') = run (st2, (rMap2, index2, incoming2 ++ reverse outgoing1'), outgoing2)
                    sentCount1' = sentCount1 + length outgoing1'
                in runBoth sentCount1' (st1', (rMap1', index1', incoming1' ++ reverse outgoing2'), []) (st2', acc2', [])

        run acc@(Term, _, _) = acc
        run acc@(Wait, (_, _, []), _) = acc
        run (state, acc@(rMap, index, incoming), outgoing)
            | isOut index = (Term, (rMap, index, incoming), outgoing)
            | otherwise = 
                let cmd = commands !! index
                    (state', acc') = execCmd state acc cmd
                in case state' of
                    (Send v) -> run (state', acc', v : outgoing)
                    Norm -> run (state', acc', outgoing)
                    _ -> (state', acc, outgoing)

execCmd :: State -> (StringIntMap, Int, [Int]) -> Cmd -> (State, (StringIntMap, Int, [Int]))
execCmd Term acc _ = (Term, acc)
execCmd _ (rMap, i, incoming) cmd = case cmd of
    (Mth _ r p op) -> (Norm, (regOp rMap r p op, i + 1, incoming))
    (Jgz p1 p2) -> 
        let (rMap', j) = doJump rMap i p1 p2 
        in (Norm, (rMap', j, incoming))
    (Rcv (Reg r)) -> 
        case incoming of
            [] -> (Wait, (rMap, i, []))
            (msg:msgs) -> (Norm, (M.insert r msg rMap, i + 1, msgs))
    (Snd p) -> 
        let (pVal, rMap') = rMap `getParamValue` p
        in (Send pVal, (rMap', i + 1, incoming))
    _ -> error "unexpected cmd"

data State = Norm | Send Int | Wait | Term deriving Show

toCmds :: String -> [Cmd]
toCmds = map (toCmd . splitOn " ") . lines

execSoundCmd :: (StringIntMap, SoundState, Int) -> Cmd -> (StringIntMap, SoundState, Int)
execSoundCmd (rMap, state, i) cmd = case cmd of
    (Mth _ r p op) -> (regOp rMap r p op, state, i + 1)
    (Jgz p1 p2) -> let (rMap', j) = doJump rMap i p1 p2 in (rMap', state, j)
    (Snd p) -> 
        let (pVal, rMap') = rMap `getParamValue` p 
        in (rMap', LastSnd pVal, i + 1)
    (Rcv p) -> 
        let (pVal, rMap') = rMap `getParamValue` p 
            (LastSnd lastSnd) = state
            state' = if pVal /= 0 then LastRcv lastSnd else state
        in (rMap', state', i + 1)

doJump :: StringIntMap -> Int -> CmdParam -> CmdParam -> (StringIntMap, Int)
doJump rMap i p1 p2 = 
    let (p1Val, rMap1) = rMap `getParamValue` p1
        (p2Val, rMap2) = rMap1 `getParamValue` p2
        j = if p1Val > 0 then i + p2Val else i + 1
    in (rMap2, j)

data SoundState = Init | LastSnd Int | LastRcv Int deriving Show

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
toCmd ["snd", v] = Snd $ toCmdParam v
toCmd ["set", r, v] = Mth "set" r (toCmdParam v) (\_ u -> u)
toCmd ["add", r, v] = Mth "add" r (toCmdParam v) (+)
toCmd ["mul", r, v] = Mth "mul" r (toCmdParam v) (*)
toCmd ["mod", r, v] = Mth "mod" r (toCmdParam v) mod
toCmd ["rcv", v] = Rcv $ toCmdParam v
toCmd ["jgz", x, y] = Jgz (toCmdParam x) (toCmdParam y)
toCmd vs = error $ "unexpected cmd: " ++ show vs

toCmdParam :: String -> CmdParam
toCmdParam s@(c:_)
    | isAlpha c = Reg s
    | otherwise = Val $ read s
toCmdParam s = error $ "unexpected cmd param: " ++ s

data Cmd = Snd CmdParam
         | Rcv CmdParam
         | Jgz CmdParam CmdParam
         | Mth String String CmdParam (Int -> Int -> Int)

instance Show Cmd where
    show (Snd p) = "Snd (" ++ show p ++ ")"
    show (Rcv p) = "Rcv (" ++ show p ++ ")"
    show (Jgz x y) = "Jgz (" ++ show x ++ ") (" ++ show y ++ ")" 
    show (Mth name r p _) = "Mth[" ++ name ++ "] " ++ r ++ " (" ++ show p ++ ")"

data CmdParam = Val Int | Reg String deriving Show


testDay18 :: Bool
testDay18 = all (test day18) 
    [ ("set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2", 4)
    ]