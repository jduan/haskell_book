module Example where

import Control.Monad.Writer

data Expr
  = Constant Bool
  | And Expr
        Expr
  | Or Expr
       Expr
  deriving (Show, Eq)

-- a straightforward interpreter
eval :: Expr -> Bool
eval (Constant b) = b
eval (And e1 e2) = eval e1 && eval e2
eval (Or e1 e2) = eval e1 || eval e2

-- Now suppose that the interpreter should also return the number of
-- operations applied. We count And and Or operations.
eval' :: Expr -> (Bool, Int)
eval' (Constant b) = (b, 0)
eval' (And e1 e2) =
  let (v1, i1) = eval' e1
      (v2, i2) = eval' e2
   in (v1 && v2, i1 + i2 + 1)
eval' (Or e1 e2) =
  let (v1, i1) = eval' e1
      (v2, i2) = eval' e2
   in (v1 || v2, i1 + i2 + 1)

-- Alas, the resulting interpreter above is harder to understand. The
-- collection of counts is entangled with the basic logic. By conversion
-- to monadic style, we can hide counting except when we need increment
-- the counter. We use the writer monad here so that we simply combine
-- counts from subexpression (as also done in the non-monadic code
-- above).
--
-- Monadic style!
evalM :: Expr -> Writer (Sum Int) Bool
evalM (Constant b) = return b
-- This is how it works!
-- 1. evalM e1 returns a "Writer (Sum Int) Bool"
-- 2. we bind that to "\b1 -> evalM e2"
--   * b1 is ignored
--   * "evalM e2" returns another "Writer (Sum Int) Bool"
--   * the Monadic behavior is to 'mappend' the "w" (ie Sum Int)
--   * the whole thing returns another "Writer" whose "w" is the 2 combined
-- 3. we bind that again to "\b2 -> tell (Sum 1)"
--   * "tell (Sum 1)" returns a "Writer ((), Sum 1)".
--   * the new Writer's "w" gets mappended with "Sum 1" again
-- 4. we then sequence >> that Writer to the final "return (b1 && b2)"
--   * "return (b1 && b2)" creates a "Writer (b1 && b2, mempty)"
--   * the final "a" will be "b1 && b2"
--   * the final "w" will be the previous "w" mappended with "mempty"
evalM (And e1 e2) =
  evalM e1 >>= \b1 -> evalM e2 >>= \b2 -> tell (Sum 1) >> return (b1 && b2)
evalM (Or e1 e2) =
  evalM e1 >>= \b1 -> evalM e2 >>= \b2 -> tell (Sum 1) >> return (b1 || b2)

evalM' :: Expr -> Writer (Sum Int) Bool
evalM' (Constant b) = return b
evalM' (And e1 e2) = do
  b1 <- evalM' e1
  b2 <- evalM' e2
  tell 1
  return (b1 && b2)
evalM' (Or e1 e2) = do
  b1 <- evalM' e1
  b2 <- evalM' e2
  tell 1
  return (b1 || b2)

main :: IO ()
main = do
  let e1 = And (Constant True) (Constant False)
  let e2 = Or (Constant False) (Constant True)
  let e3 = And e1 e2
  let e = evalM e3
  let e' = evalM' e3
  print (runWriter e)
  print (runWriter e')
