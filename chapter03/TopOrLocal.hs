module TopOrLocal where

topLevelFunc :: Integer -> Integer
topLevelFunc x = x + woot + topLevelValue
  where
    woot :: Integer
    woot = 10

topLevelValue :: Integer
topLevelValue = 5
