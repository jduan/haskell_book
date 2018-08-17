module Exercises where

import Control.Applicative ((*>))
import Control.Monad (join)

--
-- join :: Monad m => m (m a) -> m a
--
-- Write bind in terms of fmap and join!
-- keep in mind this is >>= flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

--
-- sequencing operator
--
sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"
  putStrLn "yet another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >> putStrLn "another thing" >> putStrLn "yet another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *> putStrLn "another thing" *> putStrLn "yet another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

--
-- more examples of desugaring do syntax
--
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn ("y helo thar: " ++ name)

--
-- As the nesting intensifies, you can see how "do syntax" can make things
-- a bit cleaner and easier to read:
--
twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn "age pls:" >> getLine >>= \age ->
      putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")
