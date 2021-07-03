module Funky where

import Data.Tuple

felem :: (Eq a) => a -> [a] -> Bool
felem _ [] = False
felem a (x:xs)
    | x == a = True
    | otherwise = felem a xs

fnub :: (Eq a) => [a] -> [a]
fnub [] = []
fnub [x] = [x]
fnub (x:xs) = aux (x:xs) [] 
    where
    aux [] acc = acc
    aux (x:xs) acc 
        | felem x acc = aux xs acc
        | otherwise   = aux xs (x:acc)

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:x':xs)
    | x <= x' = isAsc (x':xs)
    | otherwise = False

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] u v = u == v
hasPath (x:xs) u v 
    | u == v = True
    | fst x == u = hasPath xs (snd x) v
    | otherwise = or  [hasPath xs v' v | (u', v') <- xs, u' == u] 
