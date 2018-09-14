module AnotherTry where

import Control.Monad.State (State, evalState, execState, runState, state)
import System.Random

--
-- stack
--
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x xs = ((), x : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
  let ((), newStack1) = push 3 stack
      (a, newStack2) = pop newStack1
   in pop newStack2

pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x * 10, xs)

push' :: Int -> State Stack ()
push' x = state $ \xs -> ((), x : xs)

stackManip' :: State Stack Int
stackManip' = do
  push' 3
  a <- pop'
  pop'

main :: IO ()
main = do
  print (stackManip [5, 8, 2, 1])
  print (evalState stackManip' [5, 8, 2, 1])
  print (execState stackManip' [5, 8, 2, 1])
  print (runState stackManip' [5, 8, 2, 1])
