module Utils where

-- | Returns `True` if test is ok and throws error otherwise
test :: (Eq b, Show a, Show b) => (a -> b) -> (a, b) -> Bool
test f (arg, expected) = (actual == expected) || error errMsg
    where
        actual = f arg
        errMsg = "For arg `" ++ show arg ++ "` expected result `" ++ show expected ++ "`, got `" ++ show actual