{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function

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

--
--
-- Exercises: Heavy Lifting
--
-- 1. a = fmap (+1) $ read "[1]" :: [Int]
--
-- 2. b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
--
-- 3. c = (*2) . (\x -> x + 2)
--
-- 4. d = ((return '1' ++) . show) . (\x -> [x, 1..3])
--
-- 5. no idea
--
--
-- This is basically a "tuple" type
data Two a b =
  Two a
      b
  deriving (Show, Eq)

-- This is basically a "Either" type
data Or a b
  = First a
  | Second b
  deriving (Show, Eq)

-- "a" is the part of the functorial structure so we're not supposed to
-- touch "a"
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- "a" is the part of the functorial structure so we're not supposed to
-- touch "a"
instance Functor (Or a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

--
-- use quick check to validate Functors
--
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- (fmap f) . (fmap g) = fmap (f . g)
functorCompose :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
functorCompose m n z = ((fmap m) . (fmap n) $ z) == fmap (m . n) z

functorCompose' :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
functorCompose' (Fun _ m) (Fun _ n) f = (fmap m . fmap n) f == fmap (m . n) f

type IntToInt = Fun Int Int

type IntFC = IntToInt -> IntToInt -> [Int] -> Bool

--
--
-- Exercises: Instances of Functor
--
--
newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary (Identity Int) where
  arbitrary = frequency [(1, return $ Identity 10), (1, return $ Identity 100)]

data Pair a =
  Pair a
       a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary (Pair String) where
  arbitrary =
    frequency [(1, return $ Pair "hello" "world"), (1, return $ Pair "hi" "yo")]

-- data type "Two" is implemented further above
instance Arbitrary (Two Int String) where
  arbitrary =
    frequency [(1, return $ Two 1 "hello"), (1, return $ Two 2 "world")]

data Three a b c =
  Three a
        b
        c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Arbitrary (Three Int String String) where
  arbitrary =
    frequency
      [ (1, return $ Three 1 "hi" "hi")
      , (1, return $ Three 2 "bye" "bye")
      , (1, return $ Three 3 "mmm" "mmm")
      ]

data Three' a b =
  Three' a
         b
         b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Arbitrary (Three' Int String) where
  arbitrary =
    frequency
      [ (1, return $ Three' 1 "hi" "you")
      , (1, return $ Three' 2 "hello" "world")
      , (1, return $ Three' 3 "bad" "boy")
      ]

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance Arbitrary (Four Int String Int String) where
  arbitrary =
    frequency
      [(1, return $ Four 1 "hi" 2 "bye"), (1, return $ Four 3 "what" 4 "why")]

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Arbitrary (Four' Int String) where
  arbitrary =
    frequency [(1, return $ Four' 1 2 3 "hi"), (1, return $ Four' 4 5 6 "yo")]

--
-- 8. You can't write a Functor instance for Trivial because Trivial isn't
-- a higher kinded type!
--
--
main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+ 1) (* 2) (x :: [Int])
  quickCheck (functorCompose' :: IntFC)
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+ 1) (* 2) (x :: Identity Int)
  quickCheck $ \x -> functorIdentity (x :: Pair String)
  quickCheck $ \x -> functorCompose (++ "?") (++ "!") (x :: Pair String)
  quickCheck $ \x -> functorIdentity (x :: Two Int String)
  quickCheck $ \x -> functorCompose (++ "?") (++ "!") (x :: Two Int String)
  quickCheck $ \x -> functorIdentity (x :: Three Int String String)
  quickCheck $ \x ->
    functorCompose (++ "?") (++ "!") (x :: Three Int String String)
  quickCheck $ \x -> functorIdentity (x :: Three' Int String)
  quickCheck $ \x -> functorCompose (++ "?") (++ "!") (x :: Three' Int String)
  quickCheck $ \x -> functorIdentity (x :: Four Int String Int String)
  quickCheck $ \x ->
    functorCompose (++ "?") (++ "!") (x :: Four Int String Int String)
  quickCheck $ \x -> functorIdentity (x :: Four' Int String)
  quickCheck $ \x -> functorCompose (++ "?") (++ "!") (x :: Four' Int String)
