nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunc :: Integer -> Bool -> Integer
curriedFunc i b = i + (nonsense b)

uncurriedFunc :: (Integer, Bool) -> Integer
uncurriedFunc (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)

myid :: a -> a
myid x = x

f :: a -> a -> a
-- f x y = x
f x y = y

f2 :: a -> b -> b
f2 x y = y
