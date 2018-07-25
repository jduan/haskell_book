module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

multiply :: (Ord a, Eq a, Num a) => a -> a -> a
multiply x y
  | x == 0 || y == 0 = 0
  | x < 0 && y < 0 = go (-x) (-y) 0
  | x < 0 && y > 0 = negate $ go (-x) y 0
  | x > 0 && y < 0 = negate $ go x (-y) 0
  | x > 0 && y > 0 = go x y 0
  where
    go 0 y sum = sum
    go x y sum = go (x - 1) y (sum + y)

main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do (1 + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ do 2 + 2 `shouldBe` 4
    describe "dividedBy" $ do
      it "15 divided by 3 is 5" $ do dividedBy 15 3 `shouldBe` (5, 0)
      it "22 divided by 5 is 4 remainder 2" $ do
        dividedBy 22 5 `shouldBe` (4, 2)
    describe "multiple" $ do
      it "3 multiplied by 5 is 15" $ do multiply 3 5 `shouldBe` 15
      it "3 multiplied by -5 is -15" $ do multiply 3 (-5) `shouldBe` (-15)
      it "-3 multiplied by 5 is -15" $ do multiply (-3) 5 `shouldBe` (-15)
      it "-3 multiplied by -5 is 15" $ do multiply (-3) (-5) `shouldBe` 15
      it "3 multiplied by 0 is 0" $ do multiply 3 0 `shouldBe` 0
    describe "quick check" $ do
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)
