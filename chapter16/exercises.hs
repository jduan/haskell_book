module Exercises where

--
-- Exercises: Be Kind
--
-- 1. What's the kind of a? *
-- a -> a
--
-- 2. What are the kinds of b and T? both are * -> *
-- a -> b a -> T (b a)
--
-- 3. what's the kind of c? * -> * -> *
-- c a b -> c a b
--
--
-- The following won't compile because FixMePls isn't higher-kinded!
-- data FixMePls
--   = FixMe
--   | Pls
--   deriving (Show, Eq)
--
-- instance Functor FixMePls where
--   fmap = undefined
--
--
-- This works!
data FixMePls a
  = FixMe
  | Pls a
  deriving (Show, Eq)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls $ f a
