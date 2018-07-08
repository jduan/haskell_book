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

-- Implement standard list functions
--
--
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) = x == y || myElem x ys

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = go xs []
  where
    go [] acc = acc
    go (x:xs) acc = go xs (x : acc)

squish :: [[a]] -> [a]
squish [[]] = []
squish ([]:xs) = squish xs
squish ((y:ys):xs) = y : squish (ys : xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x] = x
myMaximumBy f (x:xs) =
  let max = myMaximumBy f xs
   in case f x max of
        LT -> max
        EQ -> max
        GT -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f (x:xs) =
  let max = myMinimumBy f xs
   in case f x max of
        LT -> x
        EQ -> x
        GT -> max

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
