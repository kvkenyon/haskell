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

rev :: [a] -> [a]
rev = foldl (\acc x -> x:acc) [] 

prefixes :: [a] -> [[a]]
prefixes xs = rev (foldl (\acc x -> (aux x acc):acc) [] xs)
    where 
        aux x [] = [x]
        aux x acc = (head acc) ++ [x]

prefixes2 :: [a] -> [[a]]
prefixes2 = foldr (\x acc -> [x] : (map ((:) x))  acc) [] 

lagrange :: [(Float, Float)] -> Float -> Float
lagrange points x = foldl (\acc (xj, yj) -> acc + yj * (l x xj points)) 0 points

l :: Float -> Float -> [(Float,Float)] -> Float
l x xj points = foldl (\acc (x',y') -> if xj /= x' 
    then acc * ((x - x') / (xj - x')) else acc * 1) 1 points 

data Trie a = Leaf a | Node a [Trie a]
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf a) = f acc a
foldtrie f acc (Node a t) =  foldl (\acc x -> foldtrie f acc x) (f acc a) t

    



