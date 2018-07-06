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

-- data DividedByData =
--   Maybe (Integer, Integer)
--
-- dividedBy :: Integral a => a -> a -> DividedByData
dividedBy :: (Ord t, Num a, Num t) => t -> t -> Maybe (a, t)
dividedBy num denom
  | denom == 0 = Nothing
  | num < 0 && denom > 0 =
    let (a, b) = go (abs (num - 2)) denom 0
     in Just (negate a, b)
  | num > 0 && denom < 0 =
    let (a, b) = go (num + 2) (negate denom) 0
     in Just (negate a, negate b)
  | num < 0 && denom < 0 =
    let (a, b) = go (negate num) (negate denom) 0
     in Just (a, negate b)
  | otherwise = Just (go num denom 0)
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

sumR :: (Eq a, Num a) => a -> a
sumR n = go n 0
  where
    go i total
      | i == 1 = total + 1
      | otherwise = go (i - 1) (total + i)

multiply :: (Integral a) => a -> a -> a
multiply m n = go m n 0
  where
    go m n total
      | m == 1 = total + n
      | otherwise = go (m - 1) n (total + n)
