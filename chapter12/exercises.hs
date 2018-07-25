import Data.List (foldl', unfoldr)

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

-- Small library for Either
--
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) acc = a : acc
    f _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right b) acc = b : acc
    f _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 f2 (Left a) = f1 a
either' f1 f2 (Right b) = f2 b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

--
-- unfolds
--
mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where
    go :: Num a => a -> [a] -> a
    go n [] = n
    go n (x:xs) = go (n + x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where
    go n [] = n
    go n (x:xs) = go (n * x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

--
-- Write your own iterate and unfoldr
--
myIterator :: (a -> a) -> a -> [a]
myIterator f a = a : myIterator f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Nothing -> []
    Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x
