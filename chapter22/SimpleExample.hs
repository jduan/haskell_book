-- This is a super simple example of the Reader Monad
module SimpleExample where

import Control.Monad.Reader
import Data.Map as M

tom :: Reader String String
tom = do
  env <- ask
  return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = runReader tomAndJerry "Who is this?"

--
-- Another example
--
hello :: Reader Int String
hello = do
  env <- ask
  return ("hello " ++ show env ++ "!")

-- Asks for an "Int" from the shared environment
-- Returns a "String" wrapped in a Reader Monad
world :: Reader Int String
world = do
  env <- ask
  return ("world " ++ show env ++ "!")

-- Asks for an "Int" from the shared environment
-- Returns a "String" wrapped in a Reader Monad
-- It uses "hello" and "world" to do the actual work!
helloWorld :: Reader Int String
helloWorld = do
  h <- hello
  w <- world
  return (h ++ w)

-- Start the Reader and provide an input of 99.
runHelloWorld = runReader helloWorld 99

-- You can also run "hello" directly.
runHello = runReader hello 99--
-- This is how the line above works:
-- When you call "runReader hello 99", you pass 99 to the "hello" monad
-- "ask" is a Monad that wraps "id", so "env" gets assigned to 99
-- then you "return" a Reader Monad that wraps a function that returns
-- "hello 99!"
