module EitherMonad where

-- years ago
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop = Shop
  { founded :: Founded
  , programmers :: Coders
  } deriving (Show, Eq)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded
                          Coders
  deriving (Show, Eq)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  years' <- validateFounded years
  coders' <- validateCoders coders
  if coders' > div years' 10
    then Left $ TooManyCodersForYears years' coders'
    else Right $ Shop years coders

--
--
-- Implement the Either Monad
--
--
data Sum a b
  = First a
  | Second b
  deriving (Show, Eq)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure a = Second a
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second b = Second (f b)

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= k = k b
