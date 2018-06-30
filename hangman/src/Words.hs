module Words
  ( randomWord'
  ) where

import System.Random (randomRIO)

type WordList = [String]

-- Return a list of strings/words from the dictionary.
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- Return a list of words between [minWordLength, maxWordLength]
gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l > minWordLength && l < maxWordLength

-- Return a random word from the words
randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord
