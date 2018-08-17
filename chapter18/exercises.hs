module Exercises where

import Control.Applicative ((*>))
import Control.Monad (join)

--
-- join :: Monad m => m (m a) -> m a
--
-- Write bind in terms of fmap and join!
-- keep in mind this is >>= flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

--
-- sequencing operator
--
sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"
  putStrLn "yet another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >> putStrLn "another thing" >> putStrLn "yet another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *> putStrLn "another thing" *> putStrLn "yet another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

--
-- more examples of desugaring do syntax
--
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn ("y helo thar: " ++ name)

--
-- As the nesting intensifies, you can see how "do syntax" can make things
-- a bit cleaner and easier to read:
--
twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn "age pls:" >> getLine >>= \age ->
      putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

--
-- List Monad
--
-- The "x <- xs" line binds individual values out of the list output, like
-- a list comprehension, giving us an "a". The "if then else" is our
-- "a -> m b".
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
  xs >>=
  (\x ->
     if even x
       then [x * x, x * x]
       else [x * x])

--
--
-- Maybe Monad
--
--
data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty -> weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>= \namey ->
    noNegative age' >>= \agey ->
      noNegative weight' >>= \weighty -> weightCheck (Cow namey agey weighty)

mkSphericalCow''' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow''' name' age' weight'
  -- note that the following won't work!
  -- fmap weightCheck maybeCow
  -- because the type you get is "Maybe (Maybe Cow)"
  -- you will have to do this instead
  -- join $ fmap weightCheck maybeCow
  --
  -- the gist is "we combine fmap and join to get Monadic bind!"
 =
  let maybeCow :: Maybe Cow
      maybeCow =
        Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'
   in maybeCow >>= weightCheck
