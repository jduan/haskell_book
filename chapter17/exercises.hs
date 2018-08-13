module Exercises where

import Control.Applicative
import Data.List (elemIndex)

--
--
-- Exercises: Lookups
--
--
-- Make the following expressions type-check.
--
--
added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x2 :: Maybe Int
x2 = elemIndex 3 [1 .. 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
-- maxed = liftA2 max' x2 y2
maxed = max' <$> x2 <*> y2

xs = [1, 2, 3]

ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
-- you can apply 'sum' to a Foldable
-- a tuple is a Foldable
-- sum (6, 5)
summed = fmap sum $ (,) <$> x3 <*> y3

--
-- Identity
-- Note that there are also:
-- Control.Monad.Identity
-- Data.Functor.Identity
--
newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

--
--
-- Constant
-- Note that there is also:
-- Data.Functor.Constant
--
--
newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Show, Eq)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure b = Constant mempty
  -- Since "b" is ignored, the function application is thrown away.
  -- It can't do anything because it can only hold onto the one value.
  -- The function "f" doesn't exist, and the "b" is a ghost. So you use
  -- this when whatever you want to do involves just throwing away a
  -- function application.
  Constant a <*> Constant a' = Constant (a `mappend` a')

--
--
-- Exercise: Fixer Upper
--
--
-- 1. const <$> Just "hello" <*> Just "world"
-- 1. pure const <*> Just "hello" <*> Just "world"
-- 1. liftA2 const (Just "hello") (Just "world")
--
-- 2. (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]
--
--
--
-- List Applicative Exercise
--
--
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

-- instance Applicative List where
--   pure a = Cons a Nil
--   Nil <*> _ = Nil
--   _ <*> Nil = Nil
--   (Cons x xs) <*> ys = fmap x ys `append` (xs <*> ys)
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil xs = xs
append xs Nil = xs
append (Cons x xs) ys = Cons x (append xs ys)

--
-- Another approach
--
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  xs <*> ys = flatMap (\x -> fmap x ys) xs

lst1 = Cons 1 (Cons 2 (Cons 3 Nil))

lst2 = Cons 4 (Cons 5 (Cons 6 Nil))

functions = Cons (+ 1) (Cons (* 2) Nil)

values = Cons 1 (Cons 2 Nil)
