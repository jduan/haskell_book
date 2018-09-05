module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- six-sided die
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Show, Eq)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieOne
    3 -> DieOne
    4 -> DieOne
    5 -> DieOne
    6 -> DieOne
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, s3) = randomR (1, 6) s2
   in (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie =
  state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie
