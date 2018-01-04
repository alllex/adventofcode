module Day8 where

import qualified Data.StringMap as M
import Data.List.Split

import Utils (getOrPut, test)

day8 :: String -> Int
day8 = maxReg . foldl upd M.empty . map (splitOn " ") . lines

day8extra :: String -> Int
day8extra = fst . foldl sub (0, M.empty) . map (splitOn " ") . lines
    where 
        sub (maxVal, rMap) cmd = let newMap = upd rMap cmd in (maxVal `max` maxReg newMap, newMap)

maxReg :: M.StringMap Int -> Int
maxReg = M.foldl max 0

upd :: M.StringMap Int -- ^ current register map
    -> [String] -- ^ next command description
    -> M.StringMap Int -- ^ updated register map
upd rMap [targetReg, op, diffVal, _, condiReg, rel, condiVal] = newMap
    where 
        (condiRegVal, condiMap) = getOrPut rMap condiReg 0
        condi = rel2fun rel condiRegVal (read condiVal)
        (targetRegVal, targetMap) = getOrPut condiMap targetReg 0
        newRegVal = if condi then op2fun op targetRegVal (read diffVal) else targetRegVal
        newMap = M.insert targetReg newRegVal targetMap

upd _ cmd = error $ "unexpected cmd: " ++ show cmd

rel2fun :: Ord a => String -> a -> a -> Bool
rel2fun ">" = (>)
rel2fun ">=" = (>=)
rel2fun "<" = (<)
rel2fun "<=" = (<=)
rel2fun "==" = (==)
rel2fun "!=" = (/=)
rel2fun rel = error $ "unexpected rel: " ++ rel

op2fun :: Num a => String -> a -> a -> a
op2fun "inc" = (+)
op2fun "dec" = (-)
op2fun op = error $ "unexpected op: " ++ op

day8Test :: Bool
day8Test = all (test day8) 
    [ ("b inc 5 if a > 1", 0)
    , ("b inc 5 if a > 1\na inc 1 if b < 5", 1)
    , ("b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1", 10)
    , ("b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10", 1)
    ]

day8extraTest :: Bool
day8extraTest = all (test day8extra) 
    [ ("b inc 5 if a > 1", 0)
    , ("b inc 5 if a > 1\na inc 1 if b < 5", 1)
    , ("b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1", 10)
    , ("b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10", 10)
    ]