module Sandbox where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

-- bottom
f :: Bool -> Int
f True = error "blah"
f False = 0

-- f2 is a partial function (the opposite is a total function)
f2 :: Bool -> Int
f2 False = 0

f3 :: Bool -> Maybe Int
f3 False = Just 0
f3 True = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- y ++ " mrow " ++ x
flippy :: String -> String -> String
flippy = flip cattyConny

-- "woops mrow " ++ x
appedCatty :: String -> String
appedCatty = cattyConny "woops"

-- x ++ " mrow haha"
flappe :: String -> String
flappe = flippy "haha"

main :: IO ()
main = do
  print (appedCatty "woohoo!" == "woops mrow woohoo!")
  print (flappe "1" == "1 mrow haha")
  print (flappe (appedCatty "2") == "woops mrow 2 mrow haha")
  print (appedCatty (flappe "blue") == "woops mrow blue mrow haha")
  print
    (cattyConny (flappe "pink") (cattyConny "green" (appedCatty "blue")) ==
     "pink mrow haha mrow green mrow woops mrow blue")
  print (cattyConny (flippy "Pugs" "are") "awesome" == "are mrow Pugs mrow awesome")
