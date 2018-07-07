eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = []
eftBool False False = [False]
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT GT = [GT]
eftOrd _ _ = []

eftGeneric :: (Enum a, Ord a) => a -> a -> [a]
eftGeneric m n
  | m > n = []
  | otherwise = reverse $ go m n []
  where
    go x y lst
      | x == y = x : lst
      | otherwise = go (succ x) y (x : lst)

eftInt :: Int -> Int -> [Int]
eftInt = eftGeneric

eftChar :: Char -> Char -> String
eftChar = eftGeneric
