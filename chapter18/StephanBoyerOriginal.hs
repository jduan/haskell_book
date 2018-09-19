-- This code doesn't compile cleanly but it's ok!
-- Long list of monad tutorials: https://wiki.haskell.org/Monad_tutorials_timeline
-- ERROR MONAD
data MightFail a
  = Result a
  | Error String
  deriving (Show)

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

main = print $ quadruple 3

-- LOGGING MONAD
data Verbose a =
  Verbose a
          String

write :: String -> Verbose ()
write s = Verbose () s

andThen :: Verbose a -> (a -> Verbose b) -> Verbose b
andThen (Verbose x s1) callback =
  let (Verbose y s2) = callback x
   in Verbose y (s1 ++ "\n" ++ s2)

greet :: String -> Verbose ()
greet name =
  write "We'll miss you," `andThen` \_ ->
    write (name ++ "! <3") `andThen` \_ ->
      write "This almost looks like imperative programming!"

main =
  let (Verbose _ output) = greet "Juliusz"
   in putStrLn output

-- STATE MONAD
data State s a =
  State (s -> (a, s))

getState :: State s s
getState = State (\s -> (s, s))

setState :: s -> State s ()
setState s = State (\_ -> ((), s))

result :: a -> State s a
result x = State (\s -> (x, s))

andThen :: State s a -> (a -> State s b) -> State s b
andThen (State f) callback =
  State $ \s1 ->
    let (x, s2) = f s1
        State g = callback x
     in g s2

multiplyState :: Int -> State Int String
multiplyState x =
  getState `andThen` \y ->
    setState 10 `andThen` \_ -- "Overwritten" on the next line!
     -> setState (x * y) `andThen` \_ -> result (show x)

main =
  let (State f) = multiplyState 3
   in print (f 4)

-- THE MONAD CLASS
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f =>
      Applicative f
  where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b -- We won't need this today.

class Applicative m =>
      Monad m
  where
  (>>=) :: m a -> (a -> m b) -> m b

-- IMPLEMENTING THE MONAD CLASS
data Verbose a =
  Verbose a
          String
  deriving (Show)

instance Functor Verbose where
  fmap f (Verbose x s) = Verbose (f x) s

instance Applicative Verbose where
  pure x = Verbose x ""
  (Verbose f s1) <*> (Verbose x s2) = Verbose (f x) (s1 ++ "\n" ++ s2)

instance Monad Verbose where
  (Verbose x s1) >>= callback =
    let (Verbose y s2) = callback x
     in Verbose y (s1 ++ "\n" ++ s2)

-- DO NOTATION: LOGGING
write :: String -> Verbose ()
write s = Verbose () s

greet :: String -> Verbose ()
greet name = do
  write "We'll miss you,"
  write (name ++ "! <3")
  write "This almost looks like imperative programming!"

main =
  let (Verbose _ output) = greet "Juliusz"
   in putStrLn output

-- DO NOTATION: STATE
multiplyState :: Int -> State Int String
multiplyState x = do
  y <- getState
  setState 10 -- "Overwritten" on the next line!
  setState (x * y)
  result (show x)

-- IO MONAD
main = do
  putStrLn "What is your first name?"
  firstName <- getLine
  putStrLn "What is your last name?"
  lastName <- getLine
  putStrLn ("Hello, " ++ firstName ++ " " ++ lastName ++ "!")

-- JUST FOR FUN: X86 ASSEMBLY MONAD!
-- http://www.stephendiehl.com/posts/monads_machine_code.html
program :: X86 ()
program = do
  push rbp
  mov rbp rsp
  pop rax
  mov rsp rbp
  pop rbp
  ret
