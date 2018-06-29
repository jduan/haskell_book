module Game where

-- String: the word we're guessing
-- [Maybe Char] the characters we've filled in so far
-- Char the letters we've guessed so far
data Puzzle =
  Puzzle String
         [Maybe Char]
         String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' $ fmap renderPuzzleChar discovered ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word discovered guessed
  where
    discovered = map (const Nothing) word
    guessed = []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just char) = char

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar
