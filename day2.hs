
import Data.List()
import Data.Maybe

main :: IO ()
main = do
    -- result <- day2_1 <$> readFile "day2.in"
    -- result <- day2_2 <$> readFile "day2.in"
    -- let result = day2_1_tests 
    -- let result = day2_2_tests 
    let result = "hi"
    print result

-- PROBLEM DAY 2-1

day2_1_tests :: Bool
day2_1_tests = let f = day2_1 in 
    (f "5 1 9 5 \n 7 5 3 \n 2 4 6 8" == 18)

day2_1 :: String -> Int
day2_1 = sum . map (\row -> let (mn, mx) = minmax row in mx - mn) . strToTable 

minmax :: Ord a => [a] -> (a, a)
minmax (x:xs) = foldl (\(mn, mx) y -> (y `min` mn, y `max` mx)) (x, x) xs

-- PROBLEM DAY 2-2

day2_2_tests :: Bool
day2_2_tests = let f = day2_2 in 
    (f "5 9 2 8\n 9 4 7 3\n 3 8 6 5" == 9)

day2_2 :: String -> Int
day2_2 = sum . map (fromMaybe 0 . findDivdiv) . strToTable

-- Finds a result of the first divdiv in the list
findDivdiv :: [Int] -> Maybe Int
findDivdiv [] = Nothing
findDivdiv [_] = Nothing
findDivdiv (x:xs@(y:ys)) = divdiv x y `orElse` findDivdiv (x:ys) `orElse` findDivdiv xs

-- Returns division result when either argument divides the other.
divdiv :: Int -> Int -> Maybe Int
divdiv x y | x `mod` y == 0 = Just $ x `div` y
           | y `mod` x == 0 = Just $ y `div` x
           | otherwise      = Nothing

-- UTILITIES

strToTable :: String -> [[Int]]
strToTable = map (map read . words) . lines 

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = if isNothing x then y else x