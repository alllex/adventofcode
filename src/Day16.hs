module Day16 where

import Data.List.Split (splitOn)
import Data.List (elemIndex)

day16 :: String -> String
day16 s = foldl (flip runCmd) ['a'..'p'] $ map toCmd $ splitOn "," s

day16extra :: String -> String
day16extra s = runN (10 ^ 9 `mod` repeatIndex) run1 initial
    where
        (repeatIndex, _) = runWhile (\(i, cs) -> 0 < i && cs == initial) (\(i, cs) -> (i + 1, run1 cs)) (0, initial)
        initial = ['a'..'p']
        run1 cs = foldl (flip runCmd) cs cmds
        cmds = map toCmd $ splitOn "," s

runWhile :: (a -> Bool) -> (a -> a) -> a -> a
runWhile p f a
    | p a = a
    | otherwise = runWhile p f $! f a

runN :: Int -> (a -> a) -> a -> a
runN 0 _ a = a
runN n f a = runN (n - 1) f $! f a

toCmd :: String -> Cmd
toCmd ('s':cs) = Spin $ read cs
toCmd ('x':cs) = uncurry Exchange $ (\[a, b] -> (read a, read b)) $ splitOn "/" cs
toCmd ('p':a:'/':b:_) = Partner a b
toCmd cmd = error $ "unexpected cmd: " ++ cmd

data Cmd = Spin Int | Exchange Int Int | Partner Char Char deriving Show

runCmd :: Cmd -> String -> String
runCmd (Spin n) cs = let (hd, tl) = splitAt (length cs - n) cs in tl ++ hd
runCmd (Exchange i j) cs = let a = cs !! i; b = cs !! j in set (set cs i b) j a
runCmd (Partner a b) cs = let (Just i) = a `elemIndex` cs; (Just j) = b `elemIndex` cs in set (set cs i b) j a

set :: [a] -> Int -> a -> [a]
set (_:xs) 0 v = v:xs
set (x:xs) n v = x : set xs (n - 1) v
set _ _ _ = error "unexpected error"