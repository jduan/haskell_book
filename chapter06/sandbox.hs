module Sandbox where

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date =
  Date DayOfWeek
       Int

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth')
      -- weekday == weekday' && dayOfMonth == dayOfMonth'
   = (weekday, dayOfMonth) == (weekday', dayOfMonth')

f :: Int -> Bool
f 1 = True
f 2 = True
f _ = False

data Identity a =
  Identity a

-- Without the "Eq a" constraint, you can't compare v and v'
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- Exercises: Eq Instances
data TisAnInteger =
  TisAnInteger

instance Eq TisAnInteger where
  (==) TisAnInteger TisAnInteger = True

data TwoIntegers =
  Two Integer
      Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i3 i4) = i1 == i3 && i2 == i4

data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) (TisAString s1) (TisAString s2) = s1 == s2
  (==) _ _ = False

-- Pair is a datatype with polymorphic parameters.
-- In other words, the parameters to the data constructor "Pair" aren't
-- specific type or typeclasses!
data Pair a =
  Pair a
       a

instance Eq a => Eq (Pair a) where
  (==) (Pair x1 y1) (Pair x2 y2) = (x1, y1) == (x2, y2)

data Tuple a b =
  Tuple a
        b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x1 y1) (Tuple x2 y2) = (x1, y1) == (x2, y2)

data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False
