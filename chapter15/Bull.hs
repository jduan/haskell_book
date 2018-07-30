-- This demonstrates that a Bool Monoid can't have False as the identity
module Bull where

import Control.Monoid
import Data.Monoid
import Test.QuickCheck

data Bull
  = Fools
  | Twoo
  deriving (Show, Eq)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: BullMappend)
  quickCheck (monoidRightIdentity :: BullMappend)
