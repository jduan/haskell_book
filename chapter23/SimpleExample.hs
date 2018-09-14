module SimpleExample where

import Control.Monad.State
  ( State
  , evalState
  , execState
  , get
  , put
  , runState
  , state
  )

increaseCount :: State Int ()
increaseCount = state $ \s -> ((), s + 1 :: Int)

count :: State Int ()
count = do
  increaseCount
  increaseCount
  increaseCount
  increaseCount
  increaseCount

-- dead simple
hello :: State Int String
hello = do
  i <- get
  put (i + 1)
  return "hello world"

-- This shows the different between:
-- * execState
-- * evalState
-- * runState
main :: IO ()
main = do
  print $ execState count 0
  print $ execState hello 0
  print $ evalState hello 0
  print $ runState hello 0
