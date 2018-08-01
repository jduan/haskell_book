{-# LANGUAGE FlexibleInstances #-}

module MonoidExercises where

-- import Data.Monoid hiding ((<>))
-- import Data.Semigroup
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
-- semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
data Trivial =
  Trivial
  deriving (Show, Eq)

-- instance Semigroup Trivial where
--   Trivial <> Trivial = Trivial
instance Monoid Trivial where
  mempty = Trivial
  mappend Trivial Trivial = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = frequency [(1, return Trivial), (1, return Trivial)]

newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity a') = Identity (mappend a a')

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

instance Arbitrary (Identity String) where
  arbitrary =
    frequency [(1, return $ Identity "hi"), (1, return $ Identity "yo")]

data Two a b =
  Two a
      b
  deriving (Show, Eq)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b) (Two a' b') = Two (a <> a') (b <> b')

type TwoAssoc
   = Two String String -> Two String String -> Two String String -> Bool

instance Arbitrary (Two String String) where
  arbitrary =
    frequency [(1, return $ Two "hi" "yo"), (1, return $ Two "ab" "cd")]

newtype BoolConj =
  BoolConj Bool
  deriving (Show, Eq)

-- instance Monoid BoolConj where
--   mempty = 
main :: IO ()
main = do
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
