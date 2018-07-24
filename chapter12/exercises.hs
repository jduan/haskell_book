-- Determine the kinds
--
-- 1. a is a polymophic type parameter
--
-- 2. a is a polymophic type parameter
--    f is a function that * -> *
--
--
-- String processing
--
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe s = unwords $ map replaceWord $ words s
  where
    replaceWord word =
      case notThe word of
        Nothing -> "a"
        Just s -> s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
  where
    go [] total = total
    go [x] total = total
    go (x:y:rest) total =
      if isVowel x
        then go (y : rest) (total + 1)
        else go (y : rest) total
    isVowel "" = False
    isVowel ('a':xs) = True
    isVowel ('e':xs) = True
    isVowel ('i':xs) = True
    isVowel ('o':xs) = True
    isVowel ('u':xs) = True
    isVowel _ = False

countVowels :: String -> Integer
countVowels s = toInteger $ length $ filter isVowel s
  where
    isVowel 'a' = True
    isVowel 'e' = True
    isVowel 'i' = True
    isVowel 'o' = True
    isVowel 'u' = True
    isVowel _ = False
