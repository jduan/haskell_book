module ChapterExercises where

-- Warm-up and review
--
--
-- 1. return 3-tuples of all possible stop-vowel-stop combinations
svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
  where
    stops = "pbtdkg"
    vowels = "aeiou"

-- only returns combinations that begin with p
svsP = [('p', y, z) | y <- vowels, z <- stops]
  where
    stops = "pbtdkg"
    vowels = "aeiou"

-- return 3-tuples of all possible noun-verb-noun combinations
nounVerbNoun nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2. Return the average word length
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3. Return the average word length in fractional
seekritFunc2 x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds
--
--
-- 1. myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny returns True if a -> Bool applied to any of the values in the
-- list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

-- 3.
--
-- This version uses folding
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> a == x || b) False

-- This version uses any
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (== x)

-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- 6.
-- write myFilter in terms of foldr
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred =
  foldr
    (\x acc ->
       if pred x
         then x : acc
         else acc)
    []

-- 7.
-- flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
-- squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

-- 9.
-- squishAgain flattens a list of lists into a list, using squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy cmp [] = Nothing
myMaximumBy cmp xs =
  Just $
  foldr
    (\x acc ->
       case cmp x acc of
         LT -> acc
         EQ -> x
         GT -> x)
    (head xs)
    xs

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy cmp [] = Nothing
myMinimumBy cmp xs =
  Just $
  foldr
    (\x acc ->
       case cmp x acc of
         LT -> x
         EQ -> acc
         GT -> acc)
    (head xs)
    xs
