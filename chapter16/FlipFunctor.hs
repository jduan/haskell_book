{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

data Tuple a b =
  Tuple a
        b
  deriving (Show, Eq)

-- If you make Tuple an instance of Functor, you can only map over "b".
-- Eg: fmap (+1) ("hi", 99) => ("hi", 100)
--
-- If you want to map over "a", you can do the following flipping trick!
-- "f" is a type constructor that takes 2 parameters
newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

-- "b" is the value you can map over
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple b a)) = Flip $ Tuple (f b) a
