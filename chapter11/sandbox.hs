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
