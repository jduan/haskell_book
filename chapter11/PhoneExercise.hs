module PhoneExercise where

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

data Key =
  Key Digit
      String
  deriving (Show, Eq)

newtype DaPhone =
  DaPhone [Key]

-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keys) char =
  if 

daPhone = DaPhone [Key '1' "", Key '2' "abc"]
