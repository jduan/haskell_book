module Sandbox where

data Mood
  = Blah
  | Woot
  deriving (Show)

changeMood Blah = Woot
changeMood _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

myAbs :: Integer -> Integer
myAbs x =
  if x >= 0
    then x
    else negate x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

x = (+)

f2 xs = x w 1
  where
    w = length xs
