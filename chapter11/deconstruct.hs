module Deconstruct where

newtype Name =
  Name String
  deriving (Show, Eq)

newtype Acres =
  Acres Int
  deriving (Show, Eq)

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show, Eq)

data Farmer =
  Farmer Name
         Acres
         FarmerType
  deriving (Show, Eq)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRecord = FarmerRecord
  { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType
  } deriving (Show, Eq)

isDairyFarmer2 :: FarmerRecord -> Bool
isDairyFarmer2 farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

-- Don't do this. It's a terrible thing for a couple of reasons.
-- First, Null is bad. You should use Maybe.
-- Second, consider the case where one has a 'Null' value and attempt to
-- call "make Null". You would get an exception.
-- data Automobile
--   = Null
--   | Car { make :: String
--         , model :: String
--         , year :: Integer }
--   deriving (Show, Eq)
--
-- When you have a product that uses record accessors, keep it separate of
-- any sum type that is wrapping it. Split it out into an independent type.
data Car = Car
  { make :: String
  , model :: String
  , year :: Integer
  } deriving (Show, Eq)

-- Now if you do 'make Null', you would receive a type error!
data Automobile
  = Null
  | Automobile Car
  deriving (Show, Eq)
