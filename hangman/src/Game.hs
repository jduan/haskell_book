module Game
  ( freshPuzzle
  , runGame
  ) where

import Control.Monad (forever, when)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)

-- String: the word we're guessing
-- [Maybe Char] the characters we've filled in so far
-- Char the letters we've guessed so far
data Puzzle =
  Puzzle String
         [Maybe Char]
         String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++ "  Guessed so far: " ++ guessed

-- Generate a fresh puzzle given a word
-- "discovered" is initialized to a list of Nothing
-- "guessed" is initialized to an empty list
freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word discovered guessed
  where
    discovered = map (const Nothing) word
    guessed = []

-- Check if a char is in the puzzle or not
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = char `elem` word

-- Check if a char is already guessed or not
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

-- Render a "char" in the discovered list of chars
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just char) = char

-- Given a char, fill it in the puzzle. Update "discovered" and "guessed"
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where
    zipper wordChar guessChar =
      if wordChar == c
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith zipper word filledInSoFar

-- Ask the user for a char, and return a new puzzle.
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

-- Exit the game if too many guesses have been made
gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
  when (length guessed > 7) $ do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ word
    exitSuccess

-- Finish the game if the word has been guessed
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $ do
    putStrLn "You win!"
    exitSuccess

-- Start a game and recusively iterate on the game
runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must be a single letter"
