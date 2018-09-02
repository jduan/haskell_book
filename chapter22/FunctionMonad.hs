module FunctionMonad where

foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+ 1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (foo r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind m k = \r -> k (m r) r
