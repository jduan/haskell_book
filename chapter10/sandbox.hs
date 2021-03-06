module Sandbox where

showFoldr =
  let xs = map show [1 .. 5]
   in foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z xs =
--   case xs of
--     [] -> z
--     (x:xs) -> f x (foldr f z xs)
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f z xs =
--   case xs of
--     [] -> z
--     (x:xs) -> foldl f (f z x) xs
--
--
showFoldl =
  let xs = map show [1 .. 5]
   in foldl (\y x -> concat ["(", y, "+", x, ")"]) "0" xs

-- Take the first 3 letters of each string value in a list of strings and
-- concatenate that result into a final string
firstThreeLetters xs = foldr f "" xs
  where
    f x acc = take 3 x ++ acc
