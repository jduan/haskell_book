module AFistFulOfMonads where

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs (left + n - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (right + n - left) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- This is like a pipeline operator! You pass a parameter to a function and
-- apply the function to the parameter.
-- λ> (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2
--     (3, 1)
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  -- we aren't not binding the monadic value with <- here, it's just like
  -- putting ">>" after the monadic value whose result we want to ignore.
  -- It's equivalent to "_ <- Nothing" but prettier.
  Nothing
  second <- landRight 2 first
  landLeft 1 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

main = do
  filename <- getLine
  contents <- readFile filename
  putStrLn contents
--
-- equivalent
-- main2 =
--   getLine >>=
--     \filename ->
--       readFile filename >>=
--         \contents ->
--           putStrLn contents
