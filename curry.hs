module MakingHaskellCurry where

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x 

palindrome :: String -> Bool
palindrome xs = reverse xs == xs

signum :: Int -> Int
signum x = if x < 0 then -1 else
            if x == 0 then 0 else 1

signum_guard :: Int -> Int
signum_guard n | n == 0 = 0
               | n < 0  = -1
               | otherwise = 1 

halves :: [a] -> ([a], [a])
halves xs = (take half xs, drop half xs)
    where half = length xs `div` 2

third_a :: [a] -> a
third_a xs = head $ tail $ tail xs 

third_b :: [a] -> a
third_b xs = xs !! 2

third_c :: [a] -> a
third_c (_:_:x:_) = x 

andy :: Bool -> Bool -> Bool
andy x y = if x == True then y else x

multy :: Int -> (Int -> (Int -> Int))
multy = \x -> (\y -> (\z -> x * y * z))

double :: Num a => a -> a
double x = x + x

luhnDouble :: Int -> Int
luhnDouble n = if n' > 9 then n' - 9 else n' 
    where n' = double n 

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [(luhnDouble a), b, (luhnDouble c), d] `mod` 10 == 0 


