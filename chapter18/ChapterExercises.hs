module ChapterExercises where

import Control.Applicative (liftA2)

--
-- Write Monad instances for the following types.
--
-- 1.
data Nope a =
  NopeDotJpg
  deriving (Show, Eq)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return a = NopeDotJpg
  NopeDotJpg >>= _ = NopeDotJpg

-- 2.
data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  Left' f <*> Left' a = Left' (f a)
  Left' a <*> Right' b = Right' b
  Right' b <*> _ = Right' b

instance Monad (PhhhbbtttEither b) where
  return = pure
  Right' b >>= _ = Right' b
  Left' a >>= k = k a

-- 3.
newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= k = k a

-- 4.
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a lst) = Cons (f a) (fmap f lst)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

append :: List a -> List a -> List a
append Nil xs = xs
append xs Nil = xs
append (Cons x xs) ys = Cons x (append xs ys)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  xs <*> ys = flatMap (\x -> fmap x ys) xs

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= k = concat' $ fmap k (Cons x xs)

lst1 = Cons 1 (Cons 2 (Cons 3 Nil))

--
-- λ> lst1 >>= (\x -> Cons x (Cons x Nil))
-- Cons 1 (Cons 1 (Cons 2 (Cons 2 (Cons 3 (Cons 3 Nil)))))
--
-- 1.
--
j :: Monad m => m (m a) -> m a
j mma = mma >>= id

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs k = flipType $ fmap k xs

-- The idea is:
-- First: fmap "[m a]" into "[m [a]]"
-- Second: fold "[m [a]]" into "m [a]"
flipType :: Monad m => [m a] -> m [a]
flipType [] = return []
-- "return []" wraps [] inside a Monad
flipType xs = foldr (\ma acc -> liftA2 (++) ma acc) (return []) ms
    -- type of "ms" is [m [a]]
    -- "return" wraps [a] inside a Monad
  where
    ms = fmap (\m -> m >>= (\x -> return [x])) xs
