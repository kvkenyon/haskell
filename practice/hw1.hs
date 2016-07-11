{--
 -
 - Solutions for CIS 194 Spring 2013
 - HW1
 -
 --}

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

-- Compute the lenght of a list of Integers
listLength :: [Integer] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength(xs)

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

allButLastDigit :: Integer -> Integer
allButLastDigit n = n `div` 10

-- Ex. 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (allButLastDigit n) ++ lastDigit n : []

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = lastDigit n : toDigitsRev (allButLastDigit n)

double :: Integer -> Integer
double n = 2*n

isSingleDigit :: Integer -> Bool
isSingleDigit n = (length (toDigits n)) == 1

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther x = if isEven (listLength x)
  then double (head x) : doubleEveryOther (tail x)
  else (head x) : double (head (tail x)) : doubleEveryOther (tail (tail x))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = if isSingleDigit x
  then x
  else sumDigits (toDigits x)
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits(xs)

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

