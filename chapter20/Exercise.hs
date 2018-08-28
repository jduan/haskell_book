{-# LANGUAGE FlexibleContexts #-}

module Exercise where

import Data.Monoid

newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Foldable Identity where
  foldr f acc (Identity a) = f a acc
  foldl f acc (Identity a) = f acc a
  foldMap f (Identity a) = f a

-- Maybe in disguise!
data Optional a
  = Nada
  | Only a
  deriving (Show, Eq)

instance Foldable Optional where
  foldr f acc Nada = acc
  foldr f acc (Only a) = f a acc
  foldl f acc Nada = acc
  foldl f acc (Only a) = f acc a
  foldMap f Nada = mempty
  foldMap f (Only a) = f a

--
-- Library Functions
--
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct $ foldMap Product xs

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (Any . (== x)) xs

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = foldr (\a b -> a == x || b) False

newtype Smaller a =
  Smaller [a]
  deriving (Show, Eq)

instance Ord a => Monoid (Smaller a) where
  mempty = Smaller []
  mappend (Smaller []) (Smaller a) = Smaller a
  mappend (Smaller a) (Smaller []) = Smaller a
  mappend (Smaller a) (Smaller b) =
    if a < b
      then Smaller a
      else Smaller b

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs =
  if null' xs
    then Nothing
    else let (Smaller ys) = foldMap (\x -> Smaller [x]) xs
             y = head ys
          in Just y

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' xs =
  if null' xs
    then Nothing
    else let (Smaller ys) =
               foldr (\a b -> Smaller [a] `mappend` b) (Smaller []) xs
             y = head ys
          in Just y

null' :: (Foldable t) => t a -> Bool
null' = foldr (\a b -> False) True

null'' :: (Foldable t) => t a -> Bool
null'' xs = not . getAny $ foldMap (\x -> Any True) xs

length' :: (Foldable t) => t a -> Int
length' xs = getSum $ foldMap (const (Sum 1)) xs

length'' :: (Foldable t) => t a -> Int
length'' = foldr (\_ b -> b + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (\x -> [x])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldr mappend mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a `mappend` b) mempty

main :: IO ()
main = do
  print $ sum' [1 .. 10] == 55
  print $ sum'' [1 .. 10] == 55
  print $ product' [1 .. 10] == 3628800
  print $ product'' [1 .. 10] == 3628800
  print $ elem' 3 [1 .. 10]
  print $ elem'' 3 [1 .. 10]
  print $ null' []
  print $ null' (Left 1)
  print $ null'' []
  print $ null'' (Left 1)
  print $ length' [] == 0
  print $ length' [1 .. 5] == 5
  print $ length'' [] == 0
  print $ length'' [1 .. 5] == 5
  print $ toList' [1 .. 5] == [1 .. 5]
  print $ fold [Sum 1, Sum 2, Sum 3, Sum 4] == Sum 10
  print $ fold [Product 1, Product 2, Product 3, Product 4] == Product 24
  print $ fold' [Sum 1, Sum 2, Sum 3, Sum 4] == Sum 10
  print $ fold' [Product 1, Product 2, Product 3, Product 4] == Product 24
  print $ foldMap' Sum [1 .. 5] == Sum 15
  print $ foldMap' Product [1 .. 5] == Product 120
