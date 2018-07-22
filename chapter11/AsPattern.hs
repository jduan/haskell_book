module AsPattern where

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf left@(x:xs) right@(y:ys) =
  if x == y
    then isSubsequenceOf xs ys
    else let ys' = dropWhile (/= x) ys
          in isSubsequenceOf left ys'

capitalizeWords :: String -> [(String, String)]
capitalizeWords str =
  let ws = words str
   in zipWith (\w1 w2 -> (w1, capitalize w2)) ws ws
  where
    capitalize word = toUpper (head word) : tail word
