module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt n = n > 3 && n < 8

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m

-- Just 15
s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1] == Just [3, 2, 1]
  print $ sequenceA [x, y] == [[m, n] | m <- x, n <- y]
  print $ sequenceA [xs, ys] == Just [6, 9]
  print $ (summed <$> ((,) <$> xs <*> ys)) == Just 15
  print $ fmap summed ((,) <$> xs <*> ys) == Just 15
  print $ bolt 7 == True
  print $ fmap bolt z == [True, False, False]
  print $ sequenceA [(> 3), (< 8), even] 7 == [True, True, False]
  print $ sequA (fromMaybe 0 s') == [True, False, False]
  print $ bolt (fromMaybe 0 s') == False
