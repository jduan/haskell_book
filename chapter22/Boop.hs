module Boop where

import Control.Applicative
import Data.Char

boop = (* 2)

doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

-- same as bip
bloop :: Integer -> Integer
bloop = fmap boop doop

-- the argument will be passed to both boop and doop in parallel,
-- and the results will be added together
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- same as bbop
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- same as bbop too!
-- This uses a monadic context.
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

cap :: String -> String
cap = map toUpper

composed :: String -> String
composed = reverse . cap

fmapped :: String -> String
fmapped = fmap reverse cap

tupled :: String -> (String, String)
tupled s = (cap s, reverse s)

tupled' :: String -> (String, String, String)
tupled' = do
  a <- cap
  b <- reverse
  c <- reverse
  return (a, b, c)

tupled'' :: String -> (String, String)
tupled'' s = ("", "") >>= (\_ -> ("", cap s)) >>= (\s' -> (s', reverse s))

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

ask :: Reader a a
ask = Reader id
