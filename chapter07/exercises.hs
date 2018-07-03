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
