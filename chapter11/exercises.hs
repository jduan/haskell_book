-- Exercises: How Does Your Garden Grow?
module Exercises where

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show, Eq)

type Gardener = String

data Garden =
  Garden Gardener
         FlowerType
  deriving (Show, Eq)

-- Gardener * FlowerType
--      = Gardener * (Gardenia + Daisy + Rose + Lilac)
--      = Gardener * Gardenia +
--          Gardener * Daisy +
--          Gardener * Rose +
--          Gardener * Lilac
data Garden2
  = Gardenia2 Gardener
  | Daisy2 Gardener
  | Rose2 Gardener
  | Lilac2 Gardener
