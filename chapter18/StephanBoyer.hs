module StephanBoyer where

--
-- error monad
--
data MightFail a
  = Result a
  | Error String
  deriving (Show, Eq)

double :: Int -> MightFail Int
double x =
  if x >= 0
    then Result (x * 2)
    else Error "The input was negative!"

checkEven :: Int -> MightFail ()
checkEven x =
  if even x
    then Result ()
    else Error "The input was odd!"

andThen :: MightFail a -> (a -> MightFail b) -> MightFail b
andThen (Result x) callback = callback x
andThen (Error msg) _ = Error msg

quadruple :: Int -> MightFail Int
quadruple x = double x `andThen` \y -> checkEven y `andThen` \_ -> double y

--
-- logging monad
--
data Verbose a =
  Verbose a
          String
  deriving (Show, Eq)

write :: String -> Verbose ()
write s = Verbose () s

andThen2 :: Verbose a -> (a -> Verbose b) -> Verbose b
andThen2 (Verbose x s1) callback =
  let (Verbose y s2) = callback x
   in Verbose y (s1 ++ "\n" ++ s2)

greet :: String -> Verbose ()
greet name =
  write "We'll miss you," `andThen2` \_ ->
    write (name ++ "! <3") `andThen2` \_ ->
      write "This looks like imperative programming!"

printHeader :: String -> IO ()
printHeader s = putStrLn $ "\n=== " ++ s ++ " ===\n"

--
-- state monad
--
-- s is the state, a is the value
newtype State s a =
  State (s -> (a, s))

-- Create a State whose value and state are both s!
getState :: State s s
getState = State (\s -> (s, s))

-- Create a State whose value is () and state is s
setState :: s -> State s ()
setState s = State (\_ -> ((), s))

-- Create s State whose value is x, and preserves the state s
result :: a -> State s a
result x = State (\s -> (x, s))

-- How does this work?
-- It returns a new State of (s -> (a, s))
-- When the new state is run/called with s,
-- it applies f to s first which returns a pair of (a, s')
-- then it applies the callback to the value a, which returns a new state
-- finally it calls the function wrapped inside the new state with s'
--
-- In other words, when the new state is run/called with s,
-- the first State transforms the input state into a value and a new state
-- the value is passed to the callback which returns another State
--   * the callback may or may not use the value passed to it!
-- finally the function wrapped inside the new State is called with the new
-- state s'
andThen3 :: State s a -> (a -> State s b) -> State s b
andThen3 (State f) callback =
  State
    (\s ->
       let (a, s') = f s
           State f' = callback a
        in f' s')

-- How does "multipleState 3" work?
-- x is bound to 3
-- x is neither the initial state s or the initial value a!
-- getState creates a State with (s, s)
-- the first `andThen3` call binds "y" to the initial value
-- "setState 10" ignores the input a, and creates a State of ((), 10)
-- the second `andThen3` passes () as the value to the next chain,
-- "setState (x * y)" can access both x (3) and y (unknown at this point)
-- the last `andThen3` passes () as the value to the next chain,
-- "result (show x)" creates a State of (\s -> ("3", x * y))
multipleState :: Int -> State Int String
multipleState x =
  getState `andThen3` \y ->
    setState 10 `andThen3` \_ ->
      setState (x * y) `andThen3` \_ -> result (show x)

addState :: State Int Int
addState =
  getState `andThen3` \x ->
    State (\s -> (x * 3, s + s)) `andThen3` \y -> State (\s -> (y * y, s * s))

main :: IO ()
main = do
  printHeader "Error Monad"
  print $ quadruple 3
  print $ quadruple (-3)
  printHeader "Loging Monad"
  let (Verbose _ output) = greet "Juliusz"
   in putStrLn output
  printHeader "State Monad"
  let (State f) = multipleState 3
   in print $ f 4 == ("3", 12)
  let (State f) = addState
   in print $ f 5 == (225, 100)
