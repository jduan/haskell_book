data Booly a
  = False'
  | True'
  deriving (Show, Eq)

-- conjunction
-- You get a warning from GHC:
-- "No explicit implementation for 'mempty'"
instance Monoid (Booly a) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'

-- Optional is Maybe in disguise!
data Optional a
  = Nada
  | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada o = o
  mappend o Nada = o
  mappend (Only a) (Only a') = Only (mappend a a')
