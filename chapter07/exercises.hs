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
