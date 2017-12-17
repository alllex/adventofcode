
import Data.List

-- DAY 4

main :: IO ()
main = do
    let result = "hi"
    -- result <- day4 <$> readFile "day4.in"
    -- result <- day4extra <$> readFile "day4.in"
    -- let result = day4Tests
    print result

-- DAY 4 PROBLEM

day4Tests :: Bool
day4Tests = let f = day4 in 
    (f "a b a \n a b c \n a ab ba" == 2) &&
    (f "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa" == 2)

day4 :: String -> Int
day4 = length . filter (not . containsDuplicates) . map words . lines

containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates [] = False
containsDuplicates (w:ws) = w `elem` ws || containsDuplicates ws

-- DAY 4 EXTRA PROBLEM

day4extra :: String -> Int
day4extra = length . filter (not . containsAnagrams) . map words . lines

containsAnagrams :: [String] -> Bool
containsAnagrams [] = False
containsAnagrams (w:ws) = any (w `isAnagramTo`) ws || containsAnagrams ws

isAnagramTo :: String -> String -> Bool
w1 `isAnagramTo` w2 = sort w1 == sort w2