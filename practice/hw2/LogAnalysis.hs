{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Char

findNextIntInString :: String -> String
findNextIntInString [] = ""
findNextIntInString (x:xs) = if (isDigit x)
  then (x:xs)
  else findNextIntInString xs 

getNextIntAsString :: String -> String
getNextIntAsString [] = ""
getNextIntAsString (x:xs) = if (isDigit x)
  then x : getNextIntAsString(xs)
  else ""

stringToInt :: String -> Int
stringToInt [] = error "Empty string"
stringToInt x = read x :: Int

skipToWhiteSpace :: String -> String
skipToWhiteSpace [] = []
skipToWhiteSpace (x:xs) = if (x == ' ')
  then (x:xs)
  else skipToWhiteSpace xs

parseInt :: String -> (Int, String)
parseInt [] = error "Empty string"
parseInt x =
  let xs = findNextIntInString x
    in let ret = (stringToInt (getNextIntAsString xs))
        in (ret, skipToWhiteSpace xs)

getMessageType :: Char -> MessageType
getMessageType 'I' = Info
getMessageType 'W' = Warning

parseMessage :: String -> LogMessage
parseMessage [] = Unknown []
parseMessage (x:xs) = if (x  == 'E') 
  then let (errorVal, logLine) = parseInt xs
         in let (timeStamp, logLine2) = parseInt logLine
           in let message = (tail logLine2)
             in LogMessage (Error errorVal) timeStamp message
  else if (x == 'I' || x == 'W')
    then let (timeStamp, logLine) = parseInt xs
      in LogMessage (getMessageType x) timeStamp (tail logLine)
  else Unknown (x:xs)

parse :: String -> [LogMessage]
parse [] = []
parse x =  parseMessage (head (lines x)) : parse (unlines (tail (lines x)))

parse' :: String -> [LogMessage]
parse' [] = []
parse' x = map (\a -> parseMessage a) (lines x)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf =  Node Leaf lm Leaf
insert lm@(LogMessage _ t _) (Node l lm2@(LogMessage _ t' _) r) = if (t < t')
  then Node (insert (lm) l) lm2 r
  else Node l lm2 (insert (lm) r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = build' (x:xs) Leaf

build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] tree = tree
build' (x:xs) tree = build' (xs) (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = if (l == Leaf)
  then lm : inOrder r
  else inOrder l ++ lm : []

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error v) _ _) = v >= 50
isSevere _ = False

getMessage :: LogMessage -> String
getMessage (Unknown _) = ""
getMessage (LogMessage _ _ m) = m

messages :: [LogMessage] -> [String]
messages [] = []
messages (x:xs) = getMessage x : messages xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = messages $ inOrder $ build $ (filter (\a -> isSevere a) x)

