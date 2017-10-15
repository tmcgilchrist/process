module Process.Time (
    currentTime
  , millisecond
  , microsecond
  , waitUntil
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           System.Clock (TimeSpec(..), Clock(Monotonic), getTime)

--
-- Time utilities that are independent of the `distributed-process` monad
--
currentTime :: MonadIO m => m TimeSpec
currentTime = liftIO $ getTime Monotonic

-- Wait until the supplied condition is fulfilled and
-- then run the action.
waitUntil :: MonadIO m
          => (TimeSpec -> Bool) -- Conditional to check
          -> (TimeSpec -> m a) -- Action to run
          -> m a
waitUntil cond action = do
  t <- currentTime

  case cond t of
    True -> (action t)
    False -> do
    liftIO $ threadDelay $ 100 * millisecond
    waitUntil cond action

millisecond :: Num a => a
millisecond = 1000

microsecond :: Num a => a
microsecond = 1000 * millisecond
