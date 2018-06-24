module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyy. what's shaking?"
    else putStrLn "pshhh."
  where
    cool = coolness == "downright frosty yo"

greetIfCool2 :: String -> IO ()
greetIfCool2 coolness =
  if cool coolness
    then putStrLn "eyyyy. what's shaking?"
    else putStrLn "pshhh."
  where
    cool v = v == "downright frosty yo"
