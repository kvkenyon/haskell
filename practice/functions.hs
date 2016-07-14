isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

hailstone :: Integer -> Integer
hailstone n
  | isEven n  = n `div` 2
  | otherwise = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq(hailstone n)

hailstoneLen :: Integer -> Integer
hailstoneLen n = listLength(hailstoneSeq n) - 1

sumPair :: (Integer, Integer) -> Integer
sumPair (x, y) = x + y

f :: Int -> Int -> Int -> Int
f x y z = x + y + z

-- Compute the lenght of a list of Integers
listLength :: [Integer] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength(xs)

-- Sum every 2
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:zx)) = (x + y) : sumEveryTwo(zx)

data Thing = Ship
           | SealingWax
           | King
           | Shoe
           | Cabbage
  deriving Show

type Name = String
type Age = Integer

data Person = Person Name Age Thing
  deriving Show

getAge :: Person -> Integer
getAge (Person _ a _) = a

showPerson :: Person -> String
showPerson p@(Person n a t) = "name = " ++ n ++ " age = " ++ show a ++ " thing = " ++ show t ++ " for " ++ show p

whatsYourAge :: Person -> String
whatsYourAge (Person _ 27 _ ) = "That's my age mate."
whatsYourAge (Person _ _ _) = "Lame age"
