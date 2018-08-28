module Exercise where

data Identity a =
  Identity a
  deriving (Show, Eq)

instance Foldable Identity where
  foldr f acc (Identity a) = f a acc
  foldl f acc (Identity a) = f acc a
  foldMap f (Identity a) = f a

-- Maybe in disguise!
data Optional a
  = Nada
  | Only a
  deriving (Show, Eq)

instance Foldable Optional where
  foldr f acc Nada = acc
  foldr f acc (Only a) = f a acc
  foldl f acc Nada = acc
  foldl f acc (Only a) = f acc a
  foldMap f Nada = mempty
  foldMap f (Only a) = f a
