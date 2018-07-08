module Cipher where

import Data.Char

-- Encrypt a message by shifting each letter to the right
caesar :: Int -> String -> String
caesar shift = map (shiftRight shift)

-- Decrypt a message by shifting each letter to the left
unCaesar :: Int -> String -> String
unCaesar shift = map (shiftLeft shift)

shiftRight distance letter
  | letter `elem` ['a' .. 'z'] = chr $ (ord letter - ord 'a' + distance) `mod` 26 + ord 'a'
  | letter `elem` ['A' .. 'Z'] = toUpper $ shiftRight distance (toLower letter)
  | otherwise = letter

shiftLeft distance = shiftRight (negate distance)
