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

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj True) (BoolConj True) = BoolConj True
  mappend (BoolConj True) (BoolConj False) = BoolConj False
  mappend (BoolConj False) (BoolConj True) = BoolConj False
  mappend (BoolConj False) (BoolConj False) = BoolConj False

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
  arbitrary =
    frequency [(1, return $ BoolConj True), (1, return $ BoolConj False)]

newtype BoolDisj =
  BoolDisj Bool
  deriving (Show, Eq)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj True) (BoolDisj True) = BoolDisj True
  mappend (BoolDisj True) (BoolDisj False) = BoolDisj True
  mappend (BoolDisj False) (BoolDisj True) = BoolDisj True
  mappend (BoolDisj False) (BoolDisj False) = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary =
    frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj False)]

newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend (Combine f) (Combine g) = Combine (\a -> f a <> g a)

combineF = Combine $ \n -> Sum (n + 1)

combineG = Combine $ \n -> Sum (n - 1)

newtype Comp a = Comp
  { unComp :: a -> a
  }

instance Monoid a => Monoid (Comp a) where
  mempty = Comp $ const mempty
  mappend (Comp f) (Comp g) = Comp (\a -> f a <> g a)

compF = Comp $ \n -> n ++ "!"

compG = Comp $ \n -> n ++ "?"

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem f') = Mem runMem
    where
      runMem s =
        let (a, s1) = f s
            (a', s2) = f' s
            -- the trick is you apply f to s first, then apply f' to that
            -- result
            (_, s3) = f' s1
         in (a <> a', s3)

f' = Mem $ \s -> ("hi", s + 1)

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
  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
