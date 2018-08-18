module MonadLaws where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer
          a
  deriving (Show, Eq)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure a = CountMe 0 a
  CountMe i f <*> CountMe i' a = CountMe (i + i') (f a)

instance Monad CountMe where
  return = pure
  CountMe i a >>= k =
    let CountMe i' a' = k a
     in CountMe (i + i') a'

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main = do
  let trigger = undefined :: CountMe (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
