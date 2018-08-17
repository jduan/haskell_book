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

--
-- contrast Applicative and Monad
--
f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' :: Integer -> Maybe (Integer, Integer, String)
doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

-- doSomething' and doSomething look similar but they return different
-- things!
doSomething ::
     Applicative f => Integer -> f (Maybe Integer, Maybe Integer, Maybe String)
doSomething n =
  let a = f n
      b =
        case a of
          Just i -> g i
          _ -> Nothing
      c =
        case b of
          Just j -> h j
          _ -> Nothing
   in pure (a, b, c)
