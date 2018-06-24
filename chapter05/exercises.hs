{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

-- Determine the type
a :: Num a => a
a = (* 9) 6

b :: Num a => (a, String)
b = head [(0, "doge"), (1, "kitteh")]

c :: (Integer, String)
c = head [(0 :: Integer, "doge"), (1, "kitteh")]

d :: Bool
d =
  if False
    then True
    else False

e :: Int
e = length [1 .. 5]

f :: Bool
f = (length [1 .. 4]) > (length "TACOCAT")

x = 5

y = x + 5

w :: Num a => a
w = y * 10

z :: Num a => a -> a
z y = y * 10

f2 :: Fractional a => a
f2 = 4 / y

x2 = "Julie"

y2 = " < 3"

z2 = "Haskell"

f3 :: String
f3 = x2 ++ y2 ++ z2

-- Does it compile?
bigNum = (^) 5 $ 10

wahoo = bigNum + 10

x4 = print

y4 = print "woohoo!"

z4 = x4 "hello world"

a2 = (+)

b2 = 5

c2 = a2 10

d2 = c2 200

a3 = 12 + b3

b3 = 10000 * d2

-- write a type signature
functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
  if (x > y)
    then True
    else False

functionS :: (a, b) -> b
functionS (x, y) = y
