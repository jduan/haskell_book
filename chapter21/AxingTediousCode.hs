module AxingTediousCode where

data Query =
  Query

data SomeObj =
  SomeObj

data IoOnlyObj =
  IoOnlyObj

data Err =
  Err

-- a decoder function that makes some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- a query that runs against DB and returns an array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- a "context initializer" that also has IO side effects
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- point-free version
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

-- new version
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)

-- original version
pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return (Left err)
    (Right res) -> do
      a <- makeIoOnlyObj res
      return (Right a)
