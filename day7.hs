
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.StringMap as M

-- DAY 7

main :: IO ()
main = do
    -- let result = "hi"
    -- result <- day7 <$> readFile "day7.in"
    result <- day7extra <$> readFile "day7.in"
    -- let result = day7Tests
    -- let result = day7ExtraTests
    print result

-- DAY 7 PROBLEM

day7 :: String -> String
day7 input = findRoot $ map parseLine $ lines input

-- DAY 7 EXTRA PROBLEM

day7extra input = findTreeOutlier edgeMap wMap root
    where
        edges = map parseLine $ lines input
        root = findRoot edges
        edgeMap = foldl (\m (p, w, cs) -> M.insert p (w, cs) m) M.empty edges
        (_, wMap) = weighChildren edgeMap root 
        -- rootCs = map (\c -> (c, getWeight wMap c)) $ getChildren edgeMap root

findTreeOutlier cMap wMap node = findOutlier children
    where
        children = map (\c -> (c, getWeight wMap c)) $ getChildren cMap node
        -- treeOutlier = case findOutlier children of 
        --                Nothing -> Nothing
        --                Just (nw, (outKey, outVal)) -> 

findOutlierTests :: Bool
findOutlierTests = let f = fst . snd . fromJust . findOutlier in 
    (f [("a", 1), ("b", 2), ("c", 2)] == "a") && 
    (f [("a", 2), ("b", 1), ("c", 2)] == "b") && 
    (f [("a", 2), ("b", 2), ("c", 1)] == "c") 

findOutlier :: [(String, Int)] -> Maybe (Int, (String, Int))
findOutlier [] = Nothing
findOutlier [_] = Nothing
findOutlier [_, _] = Nothing
findOutlier (v1@(_, c1):vs@(v2@(_, c2):v3@(_, c3):_))
        | c1 == c2 && c2 == c3 = findOutlier vs
        | c1 /= c2 && c2 == c3 = Just (c2, v1)
        | c1 == c3 && c2 /= c1 = Just (c3, v2)
        | c1 == c2 && c2 /= c3 = Just (c1, v3)

getWeight :: M.StringMap (Int, Int) -> String -> Int
getWeight wMap node = snd $ (M.!) wMap node

getChildren :: M.StringMap (Int, [String]) -> String -> [String]
getChildren edgeMap parent = snd $ (M.!) edgeMap parent

data Balance = Bal Int Int | Unbal Int deriving Show

weighChildren :: M.StringMap (Int, [String]) -> String -> (Int, M.StringMap (Int, Int))
weighChildren edgeMap parent = 
    case (M.!) edgeMap parent of
     (w, []) -> (w, M.singleton parent (w, w))
     (w, cs) -> (nodeSum, M.insert parent (w, nodeSum) nodeMap)
        where 
            weightedChildren = map (weighChildren edgeMap) cs
            childWeights = map fst weightedChildren
            nodeSum = w + sum childWeights 
            nodeMap = foldl M.union M.empty $ map snd weightedChildren
        
-- COMMON

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