module WordNumber where

import Data.List (intersperse)

-- Return the English word for single digits, such as
-- 1 -> "one"
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "bad input"

-- 123 -> [1, 2, 3]
digits :: Int -> [Int]
digits n = go n []
  where
    go n lst
      | n < 10 = n : lst
      | otherwise = go (div n 10) (mod n 10 : lst)

-- 123 -> "one-two-three"
wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" words
  where
    words = map digitToWord $ digits n
