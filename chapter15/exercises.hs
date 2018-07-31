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

data Two a b =
  Two a
      b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance Arbitrary (Two String Trivial) where
  arbitrary = return (Two "hello" Trivial)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc
   = Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool

type TwoAssoc
   = Two String Trivial -> Two String Trivial -> Two String Trivial -> Bool

data Three a b c =
  Three a
        b
        c
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

type ThreeAssoc
   = Three String String String -> Three String String String -> Three String String String -> Bool

instance Arbitrary (Three String String String) where
  arbitrary = return $ Three "hello" "world" "hi"

newtype BoolConj =
  BoolConj Bool
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj True) <> (BoolConj False) = BoolConj False
  (BoolConj False) <> (BoolConj True) = BoolConj False
  (BoolConj _) <> (BoolConj _) = BoolConj False

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
  arbitrary =
    frequency [(1, return $ BoolConj True), (1, return $ BoolConj True)]

newtype BoolDisj =
  BoolDisj Bool
  deriving (Show, Eq)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj True) = BoolDisj True
  (BoolDisj True) <> (BoolDisj False) = BoolDisj True
  (BoolDisj False) <> (BoolDisj True) = BoolDisj True
  (BoolDisj _) <> (BoolDisj _) = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary =
    frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj True)]

data Or a b
  = Fst a
  | Snd b
  deriving (Show, Eq)

instance Semigroup (Or a b) where
  (Fst a) <> (Snd b) = Snd b
  (Fst a) <> (Fst a') = Fst a'
  (Snd b) <> (Snd b') = Snd b
  (Snd b) <> (Fst a) = Snd b

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

instance Arbitrary (Or String Int) where
  arbitrary = frequency [(1, return $ Fst "hi"), (1, return $ Snd 1)]

newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\a -> f a <> g a)

combineF = Combine $ \n -> Sum (n + 1)

combineG = Combine $ \n -> Sum (n - 1)

newtype Comp a = Comp
  { unComp :: a -> a
  }

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (\a -> f a <> g a)

compF = Comp $ \n -> n ++ "!"

compG = Comp $ \n -> n ++ "?"

-- type CompAssoc = Comp String -> Comp String -> Comp String -> Bool
--
-- instance Arbitrary (Comp String) where
--   arbitrary =
--     frequency [(1, return $ Comp (\s -> "hi")), (1, return $ Comp (\s -> "yo"))]
--
--
data Validation a b
  = Failure' a
  | Success' b
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure' a) <> (Failure' a') = Failure' (a <> a')
  (Failure' a) <> (Success' b) = Failure' a
  (Success' b) <> (Failure' a) = Failure' a
  (Success' b) <> (Success' b') = Success' b

type ValidationAssoc
   = Validation String Int -> Validation String Int -> Validation String Int -> Bool

instance Arbitrary (Validation String Int) where
  arbitrary = frequency [(1, return $ Failure' "hi"), (1, return $ Success' 99)]

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Show, Eq)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Failure' a)) <> (AccumulateRight (Failure' a')) =
    AccumulateRight $ Failure' a
  (AccumulateRight (Failure' a)) <> (AccumulateRight (Success' b)) =
    AccumulateRight $ Success' b
  (AccumulateRight (Success' b)) <> (AccumulateRight (Failure' a)) =
    AccumulateRight $ Success' b
  (AccumulateRight (Success' b)) <> (AccumulateRight (Success' b')) =
    AccumulateRight $ Success' (b <> b')

type AccumulateRightAssoc
   = AccumulateRight Int String -> AccumulateRight Int String -> AccumulateRight Int String -> Bool

instance Arbitrary (AccumulateRight Int String) where
  arbitrary =
    frequency
      [ (1, return $ AccumulateRight (Failure' 1))
      , (1, return $ AccumulateRight (Success' "hi"))
      ]

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Failure' a)) <> (AccumulateBoth (Failure' a')) =
    AccumulateBoth (Failure' (a <> a'))
  (AccumulateBoth (Failure' a)) <> (AccumulateBoth (Success' b)) =
    AccumulateBoth (Failure' a)
  (AccumulateBoth (Success' b)) <> (AccumulateBoth (Failure' a)) =
    AccumulateBoth (Failure' a)
  (AccumulateBoth (Success' b)) <> (AccumulateBoth (Success' b')) =
    AccumulateBoth (Success' (b <> b'))

type AccumulateBothAssoc
   = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

instance Arbitrary (AccumulateBoth String String) where
  arbitrary =
    frequency
      [ (1, return $ AccumulateBoth (Failure' "hi"))
      , (1, return $ AccumulateBoth (Success' "yo"))
      ]

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  -- quickCheck (semigroupAssoc :: CompAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
