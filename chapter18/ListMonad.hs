-- The "x <- xs" line binds individual values out of the list output, like
-- a list comprehension, giving us an "a". The "if then else" is our
-- "a -> m b".
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
  xs >>=
  (\x ->
     if even x
       then [x * x, x * x]
       else [x * x])
