module Sandbox where

-- The output of skips is a list of lists. The first list in the output
-- should be the same as the input list. The second list in the output
-- should contain every second element from the input list. . . and the
-- nth list in the output should contain every nth element from the
-- input list.
--
-- For example:
-- given "hello"
-- it should return ["hello", "el", "l", "l", "o"]
skips :: [a] -> [[a]]
skips [] = [[]]
skips xs = map (map snd) pairsFiltered
  where
    pairsFiltered = [filter (f nth) pairs | nth <- indices]
    indices = enumFromTo 1 $ length xs
    pairs = zip indices xs
    f nth (index, x) = index `mod` nth == 0
