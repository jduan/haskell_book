module FunctionApplicative where

--
-- demo of function applicative
--
newtype HumanName =
  HumanName String
  deriving (Show, Eq)

newtype DogName =
  DogName String
  deriving (Show, Eq)

newtype Address =
  Address String
  deriving (Show, Eq)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Show, Eq)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Show, Eq)

pers :: Person
pers =
  Person (HumanName "Big Bird") (DogName "Barkey") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- with Reader, alternate
getDogR' :: Person -> Dog
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
getDogR' = liftA2 Dog dogName address
