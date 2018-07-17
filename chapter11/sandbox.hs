{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

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

data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Show, Eq)

data Airline
  = PapuAir
  | CapapultsR'Us
  | TakeYourChancesUnited
  deriving (Show, Eq)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
          Integer
  deriving (Show, Eq)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir 99

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "not a car"

-- Data constructor arities
--
-- nullary
data Example0 =
  Example0
  deriving (Show, Eq)

-- unary
data Example1 =
  Example1 Int
  deriving (Show, Eq)

-- product of Int and String
data Example2 =
  Example2 Int
           String
  deriving (Show, Eq)

data MyType =
  MyVal Int
  deriving (Show, Eq)

data Example =
  MakeExample
  deriving (Show)

data Example' =
  MakeExample' Int
  deriving (Show, Eq)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- With the GeneralizedNewtypeDeriving pragma, we don't need to define
-- an instance for the TooMany typeclass since Int defines it.
newtype Goats =
  Goats Int
  deriving (Show, Eq, TooMany)

newtype Cows =
  Cows Int
  deriving (Show, Eq)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

--
-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43
--
-- Exercises: Logic Goats
--
-- 1.
type IntString = (Int, String)

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

-- I don't fully understand how to use this
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y

data Person =
  MkPerson String
           Int
  deriving (Show, Eq)

-- record syntax
-- Defining a record means that there are now named record field accessors.
-- They're just functions that go from the product type to a member of
-- product.
-- name :: Person2 -> String
-- age :: Person2 -> Int
--
data Person2 = Person2
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

p1 = Person2 "Papu" 5

-- name p1
-- age p1
--
data Fiction =
  Fiction
  deriving (Show, Eq)

data Nonfiction =
  Nonfiction
  deriving (Show, Eq)

data BookType
  = FictionBook Fiction
  | NonfictionBook Nonfiction
  deriving (Show, Eq)

type AuthorName = String

data Author =
  Author (AuthorName, BookType)
  deriving (Show, Eq)

--
--   distributive property
--   a * (b + c) -> (a * b) + (a * c)
-- Product types distribute over sum types!
-- AuthorName * BookType
--      = AuthorName * (FictionBook + NonfictionBook)
--      = AuthorName * FictionBook + AuthorName * NonfictionBook
--
-- In other words, "Author" and "Author2" are the same type.
data Author2
  = Fiction2 AuthorName
  | Nonfiction2 AuthorName
  deriving (Show, Eq)

data Expr
  = Number Int
  | Add Expr
        Expr
  | Minus Expr
  | Mult Expr
         Expr
  | Divide Expr
           Expr
