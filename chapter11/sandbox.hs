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

data GuessWhat =
  Chickenbutt
  deriving (Show, Eq)

data Id a =
  MkId a
  deriving (Show, Eq)

data Product a b =
  Product a
          b
  deriving (Show, Eq)

data Sum a b
  = First a
  | Second b
  deriving (Show, Eq)

data RecordProduct a b = RecordProduct
  { pfirst :: a
  , psecond :: b
  } deriving (Show, Eq)

newtype NumCow =
  NumCow Int
  deriving (Show, Eq)

newtype NumPig =
  NumPig Int
  deriving (Show, Eq)

data Farmhouse =
  Farmhouse NumCow
            NumPig
  deriving (Show, Eq)

-- product type: Farmhouse' is equivalent to Farmhouse
-- Farmhouse' is a type alias.
type Farmhouse' = Product NumCow NumPig

farmhouse1 :: Farmhouse
farmhouse1 = Farmhouse (NumCow 3) (NumPig 5)

farmhouse2 :: Product NumCow NumPig
farmhouse2 = Product (NumCow 3) (NumPig 5)

newtype NumSheep =
  NumSheep Int
  deriving (Show, Eq)

data BigFarmhouse =
  BigFarmhouse NumCow
               NumPig
               NumSheep
  deriving (Show, Eq)

-- product type: BigFarmhouse' is equivalent to BigFarmhouse
-- You can nest "Product" as deepely as you wish
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

bigFarmhouse1 :: BigFarmhouse
bigFarmhouse1 = BigFarmhouse (NumCow 1) (NumPig 2) (NumSheep 3)

bigFarmhouse2 :: BigFarmhouse'
bigFarmhouse2 = Product (NumCow 1) (Product (NumPig 2) (NumSheep 3))

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name
          Age
  deriving (Show, Eq)

data PigInfo =
  PigInfo Name
          Age
          LovesMud
  deriving (Show, Eq)

data SheepInfo =
  SheepInfo Name
            Age
            PoundsOfWool
  deriving (Show, Eq)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Show, Eq)

-- sum type: Animal' is equivalent to Animal
data Animal' =
  Sum CowInfo
      (Sum PigInfo SheepInfo)

animal1 :: Animal
animal1 = Cow (CowInfo "peppa" 5)

animal2 :: Sum CowInfo b
-- Notice that you can also assign it to a more granular type
-- animal2 :: Sum CowInfo PigInfo
animal2 = First (CowInfo "george" 3)

animal3 :: Sum a (Sum PigInfo b)
-- same here
-- animal3 :: Sum String (Sum PigInfo Bool)
-- even
-- animal3 :: Sum CowInfo (Sum PigInfo SheepInfo)
-- But the following doesn't work! Why???
-- animal3 :: Animal'
animal3 = Second (First (PigInfo "peppa" 8 True))

--
-- constructing values
--
trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

-- Functions are values too so you can make the 'a' parameter of 'Id'
-- a function too!
idIdentify :: Id (a -> a)
idIdentify = MkId $ \x -> x

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter =
  Twitter
  deriving (Show, Eq)

data AskFm =
  AskFm
  deriving (Show, Eq)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.001

myRecord2 :: RecordProduct Integer Float
myRecord2 = RecordProduct {pfirst = 42, psecond = 0.001}

data OperationSystem
  = Linux
  | OpenBSD
  | Mac
  | Windows
  deriving (Show, Eq)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Show, Eq)

data Programmer = Programmer
  { os :: OperationSystem
  , lang :: ProgrammingLanguage
  } deriving (Show, Eq)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

-- We can reorder stuff when we use record syntax
feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda, os = Linux}

--
-- Exercises: Programmers
--
allOperatingSystems :: [OperationSystem]
allOperatingSystems = [Linux, OpenBSD, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer {os = os, lang = lang}
  | os <- allOperatingSystems
  , lang <- allLanguages
  ]

data ThereYet =
  There Integer
        Float
        String
        Bool
  deriving (Show, Eq)

-- who needs a "builder pattern"?
nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yussss :: ThereYet
yussss = notQuite False

data Silly a b c d =
  MkSilly a
          b
          c
          d
  deriving (Show, Eq)

-- :&: is an infix data constructor
data Product2 a b =
  a :&: b
  deriving (Show, Eq)

-- an alternative way of defining the List type
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)
