{-# LANGUAGE QuasiQuotes #-}

module Day20 where

import Text.RE.PCRE.String (matches, (*=~), re)
import Data.List (sortBy, intersectBy, partition)
import Data.Ord (Ordering(..), compare)

day20 input = slowest
    where
        props = readProperties input
        simed = iterate (map tick) props !! 1000
        sorted = sortBy sorter simed
        (slowest, _, _, _) = head sorted
        sorter (_, p, v, a) (_, p', v', a') = let pp = abs3d p; pp' = abs3d p' in pp `compare` pp'

plus3d (V3D x y z) (V3D x' y' z') = V3D (x + x') (y + y') (z + z')
tick (i, p, v, a) = let v' = v `plus3d` a in (i, p `plus3d` v', v', a)
abs3d (V3D x y z) = abs x + abs y + abs z

day20extra input = length alive
    where
        props = readProperties input
        alive = iterate (filterCollided . map tick) props !! 100
        filterCollided [] = []
        filterCollided (p@(_, pos, _, _):ps) = 
            case partition (\(_, pos', _, _) -> pos == pos') ps of
                ([], _) -> p : filterCollided ps
                (cl, ncl) -> filterCollided ncl

neg3d (V3D x y z) = V3D (-x) (-y) (-z)
minus3d a b = a `plus3d` neg3d b

collisionTime (_, p, v, a) (_, p', v', a') = roots
    where
        (V3D pdx pdy pdz, V3D vdx vdy vdz, V3D adx ady adz) = (p `minus3d` p', v `minus3d` v', a `minus3d` a')
        rootsX = roots2d adx vdx pdx
        rootsY = roots2d ady vdy pdy
        rootsZ = roots2d adz vdz pdz
        merge = intersectBy (\a b -> abs (a - b) <= 0.1)
        -- roots = rootsX `intersect` rootsY `intersect` rootsZ 
        roots = rootsX `merge` rootsY `merge` rootsZ 

roots2d a b c = filter (>0) $ roots2d' (fromIntegral a) (fromIntegral b) (fromIntegral c)

roots2d' a b c 
    | a == 0 = [-c / b]
    | b == 0 = let t = -c / a in if t < 0 then [] else [sqrt t, -(sqrt t)]
    | otherwise = let d = b * b - 4 * a * c in if d < 0 then [] else [ (-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a) ]

sqrt3d (V3D x y z) = sqrt $ fromIntegral $ x * x + y * y + z * z

data V3D = V3D Int Int Int deriving (Show, Eq)

readProperties :: String -> [(Int, V3D, V3D, V3D)]
readProperties = map (\(i, s) -> toV3Ds i $ map read $ matches $ s *=~ [re|-?\d+|]) . zip [0..] . lines 
    where
        toV3Ds i [px, py, pz, vx, vy, vz, ax, ay, az] = (i, V3D px py pz, V3D vx vy vz, V3D ax ay az)


maxBy gt (x:xs) = foldl (\acc a -> if a `gt` acc then a else acc) x xs
-- minBy cmp (x:xs) = foldl (\acc a -> if a `cmp` acc == -1 then a else acc) x xs
minBy lt (x:xs) = foldl (\acc a -> if a `lt` acc then a else acc) x xs