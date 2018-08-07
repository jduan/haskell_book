module PhoneExercise where

import Data.Char (isUpper, toLower)
import Data.List (elemIndex, find, maximumBy)
import Data.Maybe (isJust)

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

data Button =
  Button Digit
         String
  deriving (Show, Eq)

newtype Phone =
  Phone [Button]
  deriving (Show, Eq)

phone :: Phone
phone =
  Phone
    [ Button '1' ""
    , Button '2' "abc"
    , Button '3' "def"
    , Button '4' "ghi"
    , Button '5' "jkl"
    , Button '6' "mno"
    , Button '7' "pqrs"
    , Button '8' "tuv"
    , Button '9' "wxyz"
    , Button '*' ""
    , Button '0' " "
    , Button '#' ".,"
    ]

convo :: [String]
convo =
  [ "wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lok ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- Given a character (upper or lower case), return a list tuples of
-- (Digit, Presses)
reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone buttons) char =
  if isUpper char
    then ('*', 1) : go (toLower char)
    else go char
  where
    go :: Char -> [(Digit, Presses)]
    go char =
      case find isJust (taps char) of
        Nothing -> error "char can't be typed!"
        Just (Just tap) -> [tap]
    taps :: Char -> [Maybe (Digit, Presses)]
    taps char = map (`reverseTap` char) buttons

-- Return a tuple of (Digit, Presses) if the given char can be
-- typed by the button. Otherwise, return Nothing
reverseTap :: Button -> Char -> Maybe (Digit, Presses)
reverseTap (Button d s) c =
  case elemIndex c (s ++ [d]) of
    Nothing -> Nothing
    (Just n) -> Just (d, n + 1)

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

-- Given a list taps, return the total number of presses.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps pairs = sum $ map snd pairs

-- Give a message, find the "digit" you would type the most when
-- composing this message.
mostPopularLetter :: String -> Char
mostPopularLetter str = fst maxPresses
  where
    maxPresses :: (Digit, Presses)
    maxPresses = maximumBy (\p1 p2 -> compare (snd p1) (snd p2)) combined
    combined :: [(Digit, Presses)]
    combined =
      foldr
        (\(d, p) acc ->
           case findDigit d acc of
             Nothing -> (d, p) : acc
             Just (d', p') -> (d, p + p') : acc)
        []
        taps
    taps :: [(Digit, Presses)]
    taps = cellPhonesDead phone str
    findDigit :: Char -> [(Digit, Presses)] -> Maybe (Digit, Presses)
    findDigit digit = find (\(d, p) -> d == digit)

coolestLetter :: [String] -> Char
coolestLetter strs = mostPopularLetter $ concat strs

main :: IO ()
main = print $ map (cellPhonesDead phone) convo
