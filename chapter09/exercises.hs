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

-- The Fearful Symmetry
--
--
-- Separate a sentence into words
myWords :: String -> [String]
myWords s = breakString s ' '

-- Separate a string into a list o strings using a given delimiter.
breakString :: String -> Char -> [String]
breakString s delim = reverse $ go s []
  where
    go [] words = words
    go s words = go (rest s) (firstWord s : words)
    firstWord = takeWhile (/= delim)
    removeLeadingSpaces = dropWhile (== delim)
    rest s = removeLeadingSpaces $ dropWhile (/= delim) s

-- Separate a multi-line string into a list of strings
myLines :: String -> [String]
myLines s = breakString s '\n'

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
