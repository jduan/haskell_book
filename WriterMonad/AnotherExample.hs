module AnotherExample where

import Control.Monad.Writer

-- Now, to motivate the Writer monad, let’s talk about the accumulation
-- problem. Suppose we have a few different functions. Each will perform
-- some string operations we’ve assigned an arbitrary “cost” to. We want to
-- keep track of how “expensive” it was to run the full computation. We can
-- do this by using accumulator arguments to keep track of the cost we’ve
-- seen so far. We then keep passing the accumulated value along.
--
-- Calls func2 if even length, func3 and func4 if odd
func1 :: String -> (Int, String)
func1 input =
  if length input `mod` 2 == 0
    then func2 (0, input)
    else (i1 + i2, str1 ++ str2)
  where
    (i1, str1) = func3 (0, tail input)
    (i2, str2) = func4 (0, take 1 input)

-- Calls func4 on truncated version
func2 :: (Int, String) -> (Int, String)
func2 (prev, input) =
  if length input > 10
    then func4 (prev + 1, take 9 input)
    else (10, input)

-- Calls func2 on expanded version if a multiple of 3
func3 :: (Int, String) -> (Int, String)
func3 (prev, input) =
  if length input `mod` 3 == 0
    then (prev + f2resI + 3, f2resStr)
    else (prev + 1, tail input)
  where
    (f2resI, f2resStr) = func2 (prev, input ++ "ab")

func4 :: (Int, String) -> (Int, String)
func4 (prev, input) =
  if length input < 10
    then (prev + length input, input ++ input)
    else (prev + 5, take 5 input)

--
--
--
-- Use "Writer Monad"
--
--
--
func1' :: String -> (String, Sum Int)
func1' input =
  if length input `mod` 2 == 0
    then runWriter (func2' input)
    else runWriter $ do
           str1 <- func3' (tail input)
           str2 <- func4' (take 1 input)
           return (str1 ++ str2)

-- Calls func4' on truncated version
func2' :: String -> Writer (Sum Int) String
func2' input =
  if length input > 10
    then do
      tell 1
      -- without binding, this indicates >>
      func4' (take 9 input)
    else do
      tell 10
      return input

-- Calls func2' on expanded version if a multiple of 3
func3' :: String -> Writer (Sum Int) String
func3' input =
  if length input `mod` 3 == 0
    then do
      tell 3
      func2' (input ++ "ab")
    else do
      tell 1
      return $ tail input

func4' :: String -> Writer (Sum Int) String
func4' input =
  if length input < 10
    then do
      tell (Sum $ length input)
      return (input ++ input)
    else do
      tell (Sum 5)
      return (take 5 input)

main :: IO ()
main = do
  print $ func1 "hell"
  print $ func1' "hell"
