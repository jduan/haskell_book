-- Determine the kinds
--
-- 1. a is a polymophic type parameter
--
-- 2. a is a polymophic type parameter
--    f is a function that * -> *
--
--
-- String processing
--
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe s = unwords $ map replaceWord $ words s
  where
    replaceWord word =
      case notThe word of
        Nothing -> "a"
        Just s -> s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
  where
    go [] total = total
    go [x] total = total
    go (x:y:rest) total =
      if isVowel x
        then go (y : rest) (total + 1)
        else go (y : rest) total
    isVowel "" = False
    isVowel ('a':xs) = True
    isVowel ('e':xs) = True
    isVowel ('i':xs) = True
    isVowel ('o':xs) = True
    isVowel ('u':xs) = True
    isVowel _ = False

countVowels :: String -> Integer
countVowels s = toInteger $ length $ filter isVowel s

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

newtype Word' =
  Word' String
  deriving (Show, Eq)

mkWord :: String -> Maybe Word'
mkWord s =
  if len1 > len2
    then Nothing
    else Just $ Word' s
  where
    len1 = length $ filter isVowel s
    len2 = length s - len1

-- Natural numbers
data Nat
  = Zero
  | Succ Nat
  deriving (Show, Eq)

-- Natural number to Integer
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

-- Integer to Natural number
integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise =
    let (Just n) = integerToNat (i - 1)
     in Just (Succ n)

-- Small library for Maybe
--
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just a') = a'

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
  where
    f Nothing acc = acc
    f (Just a) acc = a : acc

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybes =
  if any isNothing maybes
    then Nothing
    else Just $ catMaybes maybes
  where
    isNothing Nothing = True
    isNothing _ = False
