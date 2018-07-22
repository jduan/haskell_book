module AsPattern where

import Data.Char
import Data.List
import Data.List.Split

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' left@(x:xs) right@(y:ys) =
  if x == y
    then isSubsequenceOf' xs ys
    else let ys' = dropWhile (/= x) ys
          in isSubsequenceOf' left ys'

capitalizeWords :: String -> [(String, String)]
capitalizeWords str =
  let ws = words str
   in zipWith (\w1 w2 -> (w1, capitalizeWord w2)) ws ws

capitalizeWord :: String -> String
capitalizeWord word = toUpper (head word) : tail word

capitalizeParagraph :: String -> String
capitalizeParagraph parag = intercalate "." $ map capitalize parts
  where
    parts = splitOn "." parag
        -- caplitalize first non white character
    capitalize "" = ""
    capitalize (' ':rest) = ' ' : capitalize rest
    capitalize (x:xs) = toUpper x : xs
