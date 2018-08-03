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

data WhoCares a
  = ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Show, Eq)

--
-- law-abiding instance
-- instance Functor WhoCares where
--   fmap _ ItDoesnt = ItDoesnt
--   fmap _ WhatThisIsCalled = WhatThisIsCalled
--   fmap f (Matter a) = Matter (f a)
--
--
-- law-breaking instance because the "identity" law doesn't hold
instance Functor WhoCares where
  fmap _ ItDoesnt = WhatThisIsCalled
  fmap f WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)

data CountingBad a =
  Heisenberg Int
             a
  deriving (Show, Eq)

--
-- doesn't comply with the composition law
-- because "n" is changed.
-- The fix is to stop messing with the Int. Think of anything that isn't
-- the final type argument of our "f" in Functor as being part of the
-- structure that the functions being lifted should be oblivious to.
--
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)
