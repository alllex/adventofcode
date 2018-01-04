module Utils where

import qualified Data.StringMap as M

-- | Returns value for the key if it is in the map and puts there given value otherwise.
getOrPut :: M.StringMap a -> String -> a -> (a, M.StringMap a)
getOrPut m k v = 
    case M.lookup k m of
        Nothing -> (v, M.insert k v m)
        Just old -> (old, m)

-- | Returns `True` if test is ok and throws error otherwise.
test :: (Eq b, Show a, Show b) => (a -> b) -> (a, b) -> Bool
test f (arg, expected) = (actual == expected) || error errMsg
    where
        actual = f arg
        errMsg = "For arg `" ++ show arg ++ "` expected result `" ++ show expected ++ "`, got `" ++ show actual