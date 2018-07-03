module Sandobx where

myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum

stillAFunc :: [a] -> [a] -> [a] -> [a]
stillAFunc a b c = a ++ b ++ c

addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x
  -- the x in the let shadows the parameter x
 =
  let x = 10
      y = 5
   in "the integer was: " ++ show x ++ " and y was: " ++ show y

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
