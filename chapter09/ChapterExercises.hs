module ChapterExercises where

import Data.Char

-- remove lower case characters
removeLowerCase :: String -> String
removeLowerCase = filter isUpper

-- capitalize the first letter
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- upcase every character
upcase :: String -> String
upcase = map toUpper

-- return the first letter in upper case
firstLetterUpcase :: String -> Maybe Char
firstLetterUpcase [] = Nothing
firstLetterUpcase xs = Just $ head $ capitalize xs
