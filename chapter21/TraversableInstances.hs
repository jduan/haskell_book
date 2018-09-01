{-# LANGUAGE FlexibleInstances #-}

module TraversableInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--
-- Identity
--
newtype Identity' a =
  Identity' a
  deriving (Show, Eq)

instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)

instance Foldable Identity' where
  foldMap f (Identity' a) = f a

instance Traversable Identity' where
  traverse g (Identity' a) = fmap Identity' (g a)

instance Arbitrary (Identity' Int) where
  arbitrary = frequency [(1, return $ Identity' 1), (1, return $ Identity' 2)]

instance Eq a => EqProp (Identity' a) where
  (Identity' x) =-= (Identity' y) = x `eq` y

--
-- Constant
--
newtype Constant' a b = Constant'
  { getConstant' :: a
  } deriving (Show, Eq)

instance Functor (Constant' a) where
  fmap _ (Constant' x) = Constant' x

instance Foldable (Constant' a) where
  foldMap f (Constant' a) = mempty

instance Traversable (Constant' a) where
  traverse g (Constant' a) = pure (Constant' a)

instance Arbitrary (Constant' Int Int) where
  arbitrary = frequency [(1, return $ Constant' 1), (1, return $ Constant' 3)]

instance Eq a => EqProp (Constant' a b) where
  (Constant' x) =-= (Constant' x') = x `eq` x'

--
-- Maybe
--
data Optional a
  = Nada
  | Yep a
  deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap f Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse g Nada = pure Nada
  traverse g (Yep a) = Yep <$> g a

instance Arbitrary (Optional Int) where
  arbitrary = frequency [(1, return Nada), (1, return $ Yep 99)]

instance Eq a => EqProp (Optional a) where
  Nada =-= o = Nada `eq` o
  o =-= Nada = o `eq` Nada
  (Yep x) =-= (Yep y) = x `eq` y

--
-- List
--
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f xs = mconcat (go xs)
    where
      go Nil = []
      go (Cons x xs) = f x : go xs

instance Traversable List where
  traverse g Nil = pure Nil
  traverse g (Cons x xs) = Cons <$> g x <*> traverse g xs

instance Arbitrary (List Int) where
  arbitrary =
    frequency [(1, return Nil), (1, return $ Cons 1 (Cons 2 (Cons 3 Nil)))]

instance Eq a => EqProp (List a) where
  Nil =-= o = Nil `eq` o
  o =-= Nil = o `eq` Nil
  (Cons x xs) =-= (Cons y ys)
    | x == y = xs =-= ys
    | otherwise = error "not equal"

--
-- Three
--
data Three a b c =
  Three a
        b
        c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- "g c" returns an Applicative context. Then you fmap "Three a b" to it
-- in order to put "Three a b" in the Applicative context.
instance Traversable (Three a b) where
  traverse g (Three a b c) = Three a b <$> g c

instance Arbitrary (Three Int Int Int) where
  arbitrary = frequency [(1, return $ Three 1 2 3), (1, return $ Three 4 5 6)]

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (Three a b c) =-= (Three a' b' c') = property (a == a' && b == b' && c == c')

--
-- Three'
--
data Three' a b =
  Three' a
         b
         b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = f b1 `mappend` f b2

-- "g c" returns an Applicative context. Then you fmap "Three' a b" to it
-- in order to put "Three' a b" in the Applicative context.
instance Traversable (Three' a) where
  traverse g (Three' a b1 b2) = Three' a <$> g b1 <*> g b2

instance Arbitrary (Three' Int Int) where
  arbitrary = frequency [(1, return $ Three' 1 2 3), (1, return $ Three' 4 5 6)]

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (Three' a b1 b2) =-= (Three' a' b1' b2') =
    property (a == a' && b1 == b1' && b2 == b2')

--
-- Tree
--
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) =
    foldMap f left `mappend` f a `mappend` foldMap f right

instance Traversable Tree where
  traverse g Empty = pure Empty
  traverse g (Leaf a) = Leaf <$> g a
  traverse g (Node left a right) =
    Node <$> traverse g left <*> g a <*> traverse g right

--
-- This'll suck
--
-- This doesn't work!
--
-- n looks like a function
-- data S n a =
--   S (n a)
--     a
--
-- instance (Functor n) => Functor (S n) where
--   fmap f (S na a') = S (fmap f na) (f a')
--
-- instance Foldable (S n) where
--   foldMap f (S na a') = f a'
--
-- instance (Functor n) => Traversable (S n) where
--   traverse g (S na a') = S $ (fmap g na) <*> (g a')
-- type alias
type TI = Identity'

type TI2 = Constant' Int

type TI3 = Optional

type TI4 = List

type TI5 = Three Int Int

type TI6 = Three' Int

main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
  let trigger2 = undefined :: TI2 (Int, Int, [Int])
  quickBatch (traversable trigger2)
  let trigger3 = undefined :: TI3 (Int, Int, [Int])
  quickBatch (traversable trigger3)
  let trigger4 = undefined :: TI4 (Int, Int, [Int])
  quickBatch (traversable trigger4)
  let trigger5 = undefined :: TI5 (Int, Int, [Int])
  quickBatch (traversable trigger5)
  let trigger6 = undefined :: TI6 (Int, Int, [Int])
  quickBatch (traversable trigger6)
