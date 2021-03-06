module Exercises where

import Control.Applicative
import Data.List (elemIndex)
import Test.QuickCheck.Checkers

--
--
-- Exercises: Lookups
--
--
-- Make the following expressions type-check.
--
--
added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x2 :: Maybe Int
x2 = elemIndex 3 [1 .. 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
-- maxed = liftA2 max' x2 y2
maxed = max' <$> x2 <*> y2

xs = [1, 2, 3]

ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
-- you can apply 'sum' to a Foldable
-- a tuple is a Foldable
-- sum (6, 5)
summed = fmap sum $ (,) <$> x3 <*> y3

--
-- Identity
-- Note that there are also:
-- Control.Monad.Identity
-- Data.Functor.Identity
--
newtype Identity a =
  Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity (f a)

--
--
-- Constant
-- Note that there is also:
-- Data.Functor.Constant
--
--
newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Show, Eq)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure b = Constant mempty
  -- Since "b" is ignored, the function application is thrown away.
  -- It can't do anything because it can only hold onto the one value.
  -- The function "f" doesn't exist, and the "b" is a ghost. So you use
  -- this when whatever you want to do involves just throwing away a
  -- function application.
  Constant a <*> Constant a' = Constant (a `mappend` a')

--
--
-- Exercise: Fixer Upper
--
--
-- 1. const <$> Just "hello" <*> Just "world"
-- 1. pure const <*> Just "hello" <*> Just "world"
-- 1. liftA2 const (Just "hello") (Just "world")
--
-- 2. (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]
--
--
--
-- List Applicative Exercise
--
--
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

-- instance Applicative List where
--   pure a = Cons a Nil
--   Nil <*> _ = Nil
--   _ <*> Nil = Nil
--   (Cons x xs) <*> ys = fmap x ys `append` (xs <*> ys)
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil xs = xs
append xs Nil = xs
append (Cons x xs) ys = Cons x (append xs ys)

--
-- Another approach
--
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  xs <*> ys = flatMap (\x -> fmap x ys) xs

lst1 = Cons 1 (Cons 2 (Cons 3 Nil))

lst2 = Cons 4 (Cons 5 (Cons 6 Nil))

-- "functions <*> values"
-- should return
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
functions = Cons (+ 1) (Cons (* 2) Nil)

values = Cons 1 (Cons 2 Nil)

--
--
-- ZipList Applicative Exercise
--
--
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons h t) = Cons h (take' (n - 1) t)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Show, Eq)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  ZipList' xs <*> ZipList' ys = ZipList' (go xs ys)
  -- this is the key thing!
    where
      go Nil _ = Nil
      go _ Nil = Nil
      go (Cons x xs) (Cons y ys) = Cons (x y) (go xs ys)

zlist1 = ZipList' (Cons (+ 9) (Cons (* 2) (Cons (+ 8) Nil)))

zlist2 = ZipList' (Cons 1 (Cons 2 (Cons 3 Nil)))

zlist3 = ZipList' (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))

--
-- zlist1 <*> zlist2
-- zlist1 <*> zlist3
-- both should return:
-- ZipList' (Cons 10 (Cons 4 (Cons 11 Nil)))
--
--
-- Like Either!
data Validation err a
  = Failure err
  | Success a
  deriving (Show, Eq)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a) = Success a

data Errors
  = DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Show, Eq)

success :: Validation String Int
success = Success (+ 1) <*> Success 1

failure = Success (+ 1) <*> Failure [StackOverflow]

failure' = Failure [StackOverflow] <*> Success (+ 1)

failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]

-- same as Either
instance Functor (Validation e) where
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

-- Rather than just short-circuiting when it has 2 error values, it'll use
-- the Monoid typeclass to combine them. Often this will just be a list or
-- set of errors but you can do whatever you want.
instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  Success f <*> Success a = Success (f a)
  Success f <*> Failure e = Failure e
  Failure e <*> Success f = Failure e
  Failure e <*> Failure e' = Failure (e `mappend` e')

---
--
-- Chapter Exercises
--
--
-- 1.
pure1 :: a -> [a]
pure1 a = [a]

apply1 :: [] (a -> b) -> [] a -> [] b
apply1 fs xs = fs <*> xs

-- 2.
pure2 :: a -> IO a
pure2 a = pure a

apply2 :: IO (a -> b) -> IO a -> IO b
apply2 iof ioa = iof <*> ioa

-- 3.
pure3 :: Monoid a => b -> (,) a b
pure3 b = (mempty, b)

apply3 :: Monoid t => (,) t (a -> b) -> (,) t a -> (,) t b
apply3 tf t = tf <*> t

-- 4. Not sure if this works
pure4 :: a -> (e -> a)
pure4 a = \x -> a

apply4 :: (->) x (a -> b) -> (->) x a -> (->) x b
apply4 ff fa = ff <*> fa

--
--
--
-- Write Applicative instances for the following datatypes.
--
--
-- 1.
data Pair a =
  Pair a
       a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

-- 2.
data Two a b =
  Two a
      b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  Two a b <*> Two a' b' = Two (a `mappend` a') (b b')

data Three a b c =
  Three a
        b
        c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  Three a b c <*> Three a' b' c' =
    Three (a `mappend` a') (b `mappend` b') (c c')

data Three' a b =
  Three' a
         b
         b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a b1 b2 <*> Three' a' b1' b2' =
    Three' (a `mappend` a') (b1 b1') (b2 b2')

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  Four a b c d <*> Four a' b' c' d' =
    Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (d d')

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  Four' a1 a2 a3 b <*> Four' a1' a2' a3' b' =
    Four' (a1 `mappend` a1') (a2 `mappend` a2') (a3 `mappend` a3') (b b')

--
--
-- Combinations
--
--
--
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = liftA3 (,,) as bs cs
