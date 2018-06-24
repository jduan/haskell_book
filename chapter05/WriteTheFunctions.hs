module WriteTheFunctions where

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
-- r xs = xs ++ xs
r xs = tail xs

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f x = f x
