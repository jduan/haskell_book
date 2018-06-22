module Exercises where

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs x =
  let str = "Curry is awesome"
      first = take 5 str
      second = take 2 $ drop 6 str
      third = take 7 $ drop 9 str
   in third ++ " " ++ second ++ " " ++ first
