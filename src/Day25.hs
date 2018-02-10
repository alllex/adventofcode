module Day25 where

rules :: [((State, Int), (Int, Direction, State))]
rules = 
    [ ((A, 0), (1, R, B))
    , ((A, 1), (0, L, D))
    , ((B, 0), (1, R, C))
    , ((B, 1), (0, R, F))
    , ((C, 0), (1, L, C))
    , ((C, 1), (1, L, A))
    , ((D, 0), (0, L, E))
    , ((D, 1), (1, R, A))
    , ((E, 0), (1, L, A))
    , ((E, 1), (0, R, B))
    , ((F, 0), (0, R, C))
    , ((F, 1), (0, R, E))
    ]

-- compile with -O2 to run
day25 :: Int
day25 = checksum $ applyN' 12302209 runStep (A, [], 0, [])
    where
        checksum (_, left, cur, right) = sum left + cur + sum right

applyN' :: Int -> (a -> a) -> a -> a
applyN' 0 _ a = a
applyN' n f a = applyN' (n - 1) f $! f a

runStep :: (State, [Int], Int, [Int]) -> (State, [Int], Int, [Int])
runStep (state, left, cur, right) = (state', left', cc, right')
    where
        (cur', dir, state') = snd $ head $ filter ((== (state, cur)) . fst) rules
        (left', cc, right') = 
            case dir of
                L -> if null left then ([], 0, cur':right) else (tail left, head left, cur':right)
                R -> if null right then (cur':left, 0, []) else (cur':left, head right, tail right)

data Direction = L | R deriving (Show, Eq)

data State = A | B | C | D | E | F deriving (Show, Eq)