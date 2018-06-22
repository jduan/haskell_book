module Curry where

exclaim :: String -> String
exclaim s = s ++ "!"

index4 :: String -> Char
index4 s = s !! 4

drop9 :: String -> String
drop9 s = drop 9 s
