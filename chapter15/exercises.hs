{-# LANGUAGE FlexibleInstances #-}

import Data.Semigroup
import Test.QuickCheck

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <> adv <> " as he jumped into his car " <> noun <>
  " and drove off with his " <>
  adj <>
  " wife."

madlibinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibinBetter' e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]

--
-- Chapter Exercises
--
-- Semigroup exercises
--
data Trivial =
  Trivial
  deriving (Show, Eq)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Arbitrary (Identity Trivial) where
  arbitrary = return (Identity Trivial)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc
   = Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
