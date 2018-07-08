module Cipher where

import Data.Char

-- Encrypt a message by shifting each letter to the right
caesar :: String -> Int -> String
caesar message shift = map (shiftRight shift) message

shiftRight distance letter
  | letter `elem` ['a' .. 'z'] = chr $ (ord letter - ord 'a' + distance) `mod` 26 + ord 'a'
  | letter `elem` ['A' .. 'Z'] = toUpper $ shiftRight distance (toLower letter)
  | otherwise = letter
