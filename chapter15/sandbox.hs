{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Booly a
  = False'
  | True'
  deriving (Show, Eq)

-- conjunction
-- You get a warning from GHC:
-- "No explicit implementation for 'mempty'"
instance Monoid (Booly a) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'

-- Optional is Maybe in disguise!
data Optional a
  = Nada
  | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada o = o
  mappend o Nada = o
  mappend (Only a) (Only a') = Only (mappend a a')

-- You can bind infix names for function arguments!
-- "asc (+) 1 2 3" should return True
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

type S = String

type B = Bool

-- how to run it?
-- quickCheck (monoidAssoc :: S -> S -> S -> Bool)
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull
  = Fools
  | Twoo
  deriving (Show, Eq)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

-- This demonstrates that a Bool Monoid can't have False as the identity,
-- always returning the value False, and still be a valid Monoid.
instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

--
-- Exercise: Maybe Another Monoid
--
newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Show, Eq)

instance Monoid a => Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) o = o
  mappend o (First' Nada) = o
  mappend (First' o) _ = First' o

instance Arbitrary (First' String) where
  arbitrary =
    frequency [(1, return (First' Nada)), (1, return (First' (Only "hello")))]

-- firstMappend :: First' a -> First' a -> First' a
-- firstMappend = mappend
type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main2 :: IO ()
main2 = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
