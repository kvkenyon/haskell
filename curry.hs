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

squared_sum_100 = sum [x^2 | x <- [0..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- (grid n n), x /= y]

replikate :: Int -> a -> [a]
replikate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x)) - x == x]

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

(^^^) :: Int -> Int -> Int
(^^^) 0 _ = 0
(^^^) _ 0 = 1
(^^^) x y = x * (x ^^^ (y-1)) 

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid  (x - y) y
           | otherwise = euclid (y - x) x

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x <= y  = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys


all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) | p x == False = False 
              | otherwise    = all' p xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) | p x == True = True
              | otherwise = any' p xs  


