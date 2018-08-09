module Sandbox where

import Control.Applicative

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

main :: IO ()
main = do
  print $ f 3 -- Just "hello"
  print $ g 8 -- Just "chris"
  print $ (++) <$> f 3 <*> g 7 -- Just "chrissup?"
  print $ (+) <$> h 5 <*> m 1 -- Just 9007
  print $ (+) <$> h 5 <*> m 6 -- Nothing
  print $ liftA2 (++) (g 9) (f 4) -- Just "alohajulie"
  print $ liftA2 (^) (h 5) (m 4) -- Just 60466176
  print $ liftA2 (*) (h 5) (m 4) -- Just 60
  print $ liftA2 (*) (h 1) (m 1) -- Nothing
