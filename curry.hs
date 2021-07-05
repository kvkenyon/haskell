module MakingHaskellCurry where

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x 

palindrome :: String -> Bool
palindrome xs = reverse xs == xs
