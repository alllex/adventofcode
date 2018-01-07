module Day12 where

import qualified Data.StringMap as M
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List (foldl')

import Utils (test)

day12 :: String -> Int
day12 s = Set.size $ findComponent eMap "0"
    where
        eMap = M.fromList $ map parseEdges $ lines s

day12extra :: String -> Int
day12extra s = fst $ foldl' sub (0, Set.empty) vs
    where
        sub acc@(cCount, visited) v 
            | v `Set.member` visited = acc
            | otherwise = (cCount + 1, visited `Set.union` findComponent eMap v)
        eList = map parseEdges $ lines s
        vs = map fst eList
        eMap = M.fromList eList 

findComponent :: M.StringMap [String] -> String -> Set.Set String
findComponent eMap = dfs eMap Set.empty

dfs :: M.StringMap [String]  -- ^ edge map
    -> Set.Set String -- ^ visiting
    -> String  -- ^ current vertex
    -> Set.Set String -- ^ total visited
dfs eMap visiting v = Set.insert v $ foldl' sub Set.empty tos
    where
        sub acc u = acc `Set.union` visit u
        visit u = dfs eMap (Set.insert u visiting) u
        tos = filter (not . (`Set.member` visiting)) $ eMap M.! v

parseEdges :: String -> (String, [String])
parseEdges s = (parentStr, splitOn ", " childrenStr)
    where
        [parentStr, childrenStr] = splitOn " <-> " s

testDay12 :: Bool
testDay12 = all (test day12) 
    [ ("0 <-> 0", 1)
    , ("0 <-> 1\n1 <-> 0", 2)
    , ("0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5", 6)
    ]