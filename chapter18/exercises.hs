module Exercises where

import Control.Monad (join)

--
-- join :: Monad m => m (m a) -> m a
--
-- Write bind in terms of fmap and join!
-- keep in mind this is >>= flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m
