
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

day7extra input = weighChildren edgeMap root 
    where
        edges = map parseLine $ lines input
        root = findRoot edges
        edgeMap = foldl (\m (p, w, cs) -> M.insert p (w, cs) m) M.empty edges

data Balance = Bal Int Int | Unbal Int deriving Show

weighChildren :: M.StringMap (Int, [String]) -> String -> Balance
weighChildren edgeMap parent = 
    case (M.!) edgeMap parent of
     (w, []) -> Bal w w
     (w, cs) -> fromMaybe undefined unbal
      where 
        cw = map (weighChildren edgeMap) cs 
        balChildren = map (\(Bal w cSum) -> (w, cSum)) $ filter (not . isUnbal) cw
        unbal = find isUnbal cw
        isUnbal (Unbal _) = True
        isUnbal _ = False

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