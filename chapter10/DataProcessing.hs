module DataProcessing where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 100
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = foldr f [] items
  where
    f item acc =
      case item of
        DbDate utcTime -> utcTime : acc
        _ -> acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = foldr f [] items
  where
    f item acc =
      case item of
        DbNumber num -> num : acc
        _ -> acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = maximum $ filterDbDate items

sumDb :: [DatabaseItem] -> Integer
sumDb items = sum $ filterDbNumber items

avgDb :: [DatabaseItem] -> Double
avgDb items =
  let nums = filterDbNumber items
   in fromIntegral (sum nums) / fromIntegral (length nums)
