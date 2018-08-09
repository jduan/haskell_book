module MaybeApplicative where

--
--
-- Maybe Applicative
--
--
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name =
  Name String
  deriving (Show, Eq)

newtype Address =
  Address String
  deriving (Show, Eq)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = fmap Address $ validateLength 100 s

data Person =
  Person Name
         Address
  deriving (Show, Eq)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a =
  case (,) <$> mkName n <*> mkAddress a of
    Nothing -> Nothing
    Just (n', a') -> Just $ Person n' a'

-- even better
mkPerson'' :: String -> String -> Maybe Person
mkPerson'' n a = Person <$> mkName n <*> mkAddress a

--
--
-- Another example
--
--
data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight
-- better formatting
-- Cow <$> noEmpty name
--     <*> noNegative age
--     <*> noNegative weight
--
-- or use liftA3
-- liftA3 Cow (noEmpty name)
--            (noNegative age)
--            (noNegative weight)
 = Cow <$> noEmpty name <*> noNegative age <*> noNegative weight
