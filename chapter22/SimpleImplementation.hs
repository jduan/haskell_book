module SimpleImplementation where

import Control.Applicative
import Control.Monad

-- A type that wraps a function inside.
newtype R e a =
  R (e -> a)

instance Functor (R e) where
  fmap f (R ea) = R (f . ea)

instance Applicative (R e) where
  pure a = R (const a)
  -- "f x" gives you "a -> b"
  -- "ea x" gives you "a"
  -- so "f x (ea x)" gives you "b"
  R f <*> R ea = R (\x -> (f x) (ea x))

-- Remember in Monad's definition of
-- (>>=) :: m a -> (a -> m b) -> m b
-- m is the structure. In the case of "R e a", m would be "R e".
-- "runR x e" gives "a".
-- "f (runR x e)" gives a new "f", ie a new "R e a".
-- "runR newF e" applies "e" to the new "R e b" so we can get back a new
-- "b".
instance Monad (R e) where
  return = pure
  x >>= f =
    R
      (\e ->
         let newF = f (runR x e)
          in runR newF e)

-- There's no standard way to extract a value from a monad, which means
-- that for the Reader instance to be useful we will need a function to
-- actually run the Reader in a given environment and return the result.
runR :: R e a -> e -> a
runR (R f) e = f e

ask' :: R a a
-- ask' = R id
ask' = R (\e -> e)
