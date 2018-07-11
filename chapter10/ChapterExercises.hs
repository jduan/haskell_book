module ChapterExercises where

-- 1. return 3-tuples of all possible stop-vowel-stop combinations
svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
  where
    stops = "pbtdkg"
    vowels = "aeiou"

-- only returns combinations that begin with p
svsP = [('p', y, z) | y <- vowels, z <- stops]
  where
    stops = "pbtdkg"
    vowels = "aeiou"

-- return 3-tuples of all possible noun-verb-noun combinations
nounVerbNoun nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2. Return the average word length
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3. Return the average word length in fractional
seekritFunc2 x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
