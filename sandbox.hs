module Sandbox where

import Control.Applicative
import Data.Char
import Data.List

-- Like Either!
data Validation err a
  = Failure err
  | Success a
  deriving (Show, Eq)

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

data CMaybe a
  = CNothing
  | CJust Int
          a
  deriving (Show, Eq)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust i a) = CJust (i + 1) (f a)

apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply Nothing _ = Nothing
apply _ Nothing = Nothing
apply (Just f) (Just a) = Just (f a)

myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

addStuff' :: Int -> String
addStuff' = do
  a <- (* 2)
  b <- (++ "!") . show
  return (show a ++ b)

maybeFunc1 :: String -> Either String Int
maybeFunc1 "" = Left "String cannot be empty!"
maybeFunc1 str = Right $ length str

newtype CustomError =
  CustomError String
  deriving (Show, Eq)

maybeFunc2 :: Int -> Either CustomError Float
maybeFunc2 i =
  if i `mod` 2 == 0
    then Left $ CustomError "Length cannot be even!"
    else Right (fromIntegral i * 3.14159)

-- This demos how to massage "maybeFunc2" so it can be used in runMaybeFuncs
maybeFunc2' :: Either CustomError Float -> Either String Float
maybeFunc2' (Left (CustomError s)) = Left s
maybeFunc2' (Right f) = Right f

maybeFunc3 :: Float -> Either String [Int]
maybeFunc3 f =
  if f > 15.0
    then Left "Float is too large!"
    else Right [floor f, ceiling f]

runMaybeFuncs :: String -> Either String [Int]
runMaybeFuncs input = do
  i <- maybeFunc1 input
  f <- maybeFunc2' $ maybeFunc2 i
  maybeFunc3 f

repeatN 0 a = return ()
repeatN n a = a >> repeatN (n - 1) a

test = repeatN 3 $ do putStrLn "TEST"

repeatN' 0 a = return ()
repeatN' n a = (a n) >> repeatN' (n - 1) a

test' = repeatN' 3 $ \i -> do putStrLn $ "TEST: " ++ (show i)

main = do
  putStrLn "hello, world"
  test
  test'
