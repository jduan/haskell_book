module Vigenere where

import Data.Char

shift :: Char -> Char -> Char
shift char key_char =
  let o1 = ord char
      o2 = ord key_char
      diff1 = o1 - ord 'A'
      diff2 = o2 - ord 'A'
      distance = (diff1 + diff2) `mod` 26
   in chr $ ord 'A' + distance

encode :: String -> String -> String
encode message keyword = go message (cycle keyword)
  where
    go "" _ = ""
    go (' ':rest) key = ' ' : go rest key
    go (char:rest) key = shift char (head key) : go rest (tail key)

testEncode :: IO ()
testEncode =
  if encode "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
    then putStrLn "passed"
    else putStrLn "failed"
