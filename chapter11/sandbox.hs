module Sandbox where

data PugType =
  PugData

-- Since the type argument `a` doesn't occur as an argument to HuskyData,
-- that means `a` is phantom or "has no witness"
data HuskyType a =
  HuskyData

-- Both the "type constructor" and the "data constructor" are called
-- DogueDeBordeaux but they aren't the same thing.
data DogueDeBordeaux doge =
  DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[Int]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

--
-- This will not work because 10 isn't a String
-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10
--
data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)
