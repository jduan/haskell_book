module Exercises where

addOneIfOdd n =
  case odd n of
    True -> f n
    False -> n
  where
    f = \n -> n + 1

addFive x y =
  (if x > y
     then y
     else x) +
  5

addFive2 =
  \x ->
    \y ->
      (if x > y
         then y
         else x) +
      5

mflip f = \x -> \y -> f y x

mflip2 f x y = f y x

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = (flip dodgy) 2

-- let's write code
tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where
    (xLast, _) = x `divMod` 100
    (_, d) = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  if b
    then x
    else y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b =
  case b of
    True -> x
    _ -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip x = read (show x)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show
