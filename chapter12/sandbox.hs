ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
  if even n
    then Just (n + 2)
    else Nothing

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show, Eq)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Show, Eq)

mkPerson2 :: Name -> Age -> Either PersonInvalid Person
mkPerson2 name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

-- checking functions
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
  | name /= "" = Right name
  | otherwise = Left [NameEmpty]

mkPerson3 :: Name -> Age -> Either [PersonInvalid] Person
mkPerson3 name age =
  case (nameOkay name, ageOkay age) of
    (Left errors, Left errors2) -> Left $ errors ++ errors2
    (Left errors, Right age) -> Left errors
    (Right name, Left errors) -> Left errors
    (Right name, Right age) -> Right $ Person name age
