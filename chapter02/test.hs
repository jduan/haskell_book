module Test where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!!!!")

triple :: Num a => a -> a
triple x = x * 3

area r = pi * r ^ 2

-- Exercises: Parentheses and Association
x1 = 8 + 7 * 9 == 71

x2 = (8 + 7) * 9 == 135

perimeter x y = (x * 2) + (y * 2)

perimeter2 x y = x * 2 + y * 2

f x = x / 2 + 9

f2 x = x / (2 + 9)

foo x =
  let y = x * 2
      z = x ^ 2
   in 2 * y * z
