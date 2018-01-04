module Day7 where

import Data.List
import qualified Data.Set as Set
import qualified Data.StringMap as M


day7 :: String -> String
day7 input = findRoot $ map parseLine $ lines input

day7extra :: String -> Int
day7extra input = findTreeOutlier root rootTreeWeight
    where
        edges = map parseLine $ lines input
        root@(WeightedNode _ _ rootTreeWeight) = mkWeighted $ findRoot edges
        cMap = foldl (\m (p, w, cs) -> M.insert p (w, cs) m) M.empty edges
        wMap :: M.StringMap (Int, Int)
        wMap = snd $ weighChildren cMap $ nodeIdOf root 
        mkWeighted :: String -> WeightedNode
        mkWeighted nId = let (w, tw) = wMap M.! nId in WeightedNode nId w tw
        findTreeOutlier (WeightedNode pId pW pTw) expectedParentTotalWeight = 
            let children = (map mkWeighted $ cMap `getChildren` pId) in
            case findOutlier children of
                Nothing -> pW + (expectedParentTotalWeight - pTw)
                Just (expectedChildTreeWeight, outlier) -> findTreeOutlier outlier expectedChildTreeWeight

data WeightedNode = WeightedNode { nodeIdOf :: String, nodeWeightOf :: Int, nodeTreeWeightOf :: Int } 

instance Show WeightedNode where
    show (WeightedNode i w tw) = i ++ "(w=" ++ show w ++ ",tw=" ++ show tw ++ ")"

findOutlier :: [WeightedNode] -- ^ list of weighted nodes
            -> Maybe (Int, WeightedNode) -- ^ normal tree weight and the outlier
findOutlier (v1:vs@(v2:v3:_)) = case () of
      _ | c1 == c2 && c2 == c3 -> findOutlier vs
        | c1 /= c2 && c2 == c3 -> Just (c2, v1)
        | c1 == c3 && c2 /= c1 -> Just (c3, v2)
        | c1 == c2 && c2 /= c3 -> Just (c1, v3)
    where 
        c1 = nodeTreeWeightOf v1
        c2 = nodeTreeWeightOf v2
        c3 = nodeTreeWeightOf v3

findOutlier _ = Nothing

getChildren :: M.StringMap (Int, [String]) -> String -> [String]
getChildren edgeMap parent = snd $ (M.!) edgeMap parent

data Balance = Bal Int Int | Unbal Int deriving Show

weighChildren :: M.StringMap (Int, [String]) -- ^ mapping from node name to node's own weight and children list
              -> String -- ^ node for which to weigh children
              -> (Int, M.StringMap (Int, Int)) -- ^ nodes total weight and mapping for (all) chilren total weights
weighChildren edgeMap parent = 
    case (M.!) edgeMap parent of
     (w, []) -> (w, M.singleton parent (w, w))
     (w, cs) -> (nodeSum, M.insert parent (w, nodeSum) nodeMap)
        where 
            weightedChildren = map (weighChildren edgeMap) cs
            childWeights = map fst weightedChildren
            nodeSum = w + sum childWeights 
            nodeMap = foldl M.union M.empty $ map snd weightedChildren

findRoot :: Ord a => [(a, b, [a])] -> a
findRoot edges = head . Set.toList $ parents `Set.difference` children
    where
        parents = Set.fromList $ map (\(p, _, _) -> p) edges
        children = Set.fromList $ concatMap (\ (_, _, cs) -> cs) edges

parseLine :: String -> (String, Int, [String])
parseLine line = (parent, weight, children)
    where
        (parentSide, childrenSide) = " -> " `splitAtStr` line
        children = ", " `splitStr` childrenSide 
        (parent, weightAndBrace) = " (" `splitAtStr` parentSide
        weight = read $ take (length weightAndBrace - 1) weightAndBrace

splitStr :: String -> String -> [String]
splitStr _ [] = []
splitStr del s = case splitAtStr del s of
                  (l, []) -> [l]
                  (l, r) -> l : splitStr del r

splitAtStr :: String -> String -> (String, String)
splitAtStr _ [] = ([], [])
splitAtStr del (c:cs) | del `isPrefixOf` cs = ([c], drop (length del) cs)
                      | otherwise = let (l, r) = splitAtStr del cs in (c:l, r)
