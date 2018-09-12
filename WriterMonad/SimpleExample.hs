module SimpleExample where

import Control.Monad.Writer

newtype LogEntry = LogEntry
  { msg :: String
  } deriving (Show, Eq)

calc :: Writer [LogEntry] Integer
calc = do
  output "start"
  let x = sum [1 .. 100]
  output (show x)
  output "done"
  return x

output :: String -> Writer [LogEntry] ()
output x = tell [LogEntry x]

main :: IO ()
main = print $ runWriter calc
