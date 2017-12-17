
-- DAY 5

main :: IO ()
main = do
    let result = "hi"
    -- result <- day5 <$> readFile "day5.in"
    -- result <- day5extra <$> readFile "day5.in"
    -- let result = day5Tests
    -- let result = day5ExtraTests
    print result

-- DAY 5 PROBLEM

day5Tests :: Bool
day5Tests = let f = day5 in
    (f "0 3 \n0 1 -3" == 5)

day5 :: String -> Int
day5 = jumpOut (+1) . tp . map read . words

-- DAY 5 EXTRA PROBLEM

day5ExtraTests :: Bool
day5ExtraTests = let f = day5extra in
    (f "0 3 0 1 -3" == 10)

day5extra :: String -> Int
day5extra = jumpOut (\n -> if n >= 3 then n - 1 else n + 1) . tp . map read . words

tp :: [Int] -> State
tp = Tape 0 []

data State = Result Int | Tape Int [Int] [Int] deriving Show

jumpOut :: (Int -> Int) -> State -> Int
jumpOut _ (Result t) = t
jumpOut f s = jumpOut f $ jumpAndDo s f

jumpOutRecord :: (Int -> Int) -> State -> [State]
jumpOutRecord _ (Result _) = []
jumpOutRecord f s = let next = jumpAndDo s f in next : jumpOutRecord f next

jumpAndDo :: State -> (Int -> Int) -> State
jumpAndDo (Tape t ls (r:rs)) f = move r (Tape (t+1) ls (f r : rs))
jumpAndDo s _ = s

move :: Int -> State -> State
move 0 s = s 
move n (Tape t ls rs) | n < 0 && null ls = Result t
                      | n > 0 && length rs == 1 = Result t
                      | n > 0 = move (n - 1) (Tape t (head rs : ls) (tail rs))
                      | n < 0 = move (n + 1) (Tape t (tail ls) (head ls : rs))
