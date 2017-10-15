{-# LANGUAGE OverloadedStrings #-}
module Process.Logging (
    LogLevel (..)
  , logMessage
  , logErrorAndExit
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Text hiding (foldr)
import qualified Data.Text.IO as TIO
import           Data.Monoid
import           System.Exit (exitFailure)
import           System.IO (stderr, stdout, Handle)

-- Different logging levels
data LogLevel =
    Info         -- Informational messages
  | Error        -- Error messages
  | Regular      -- Expected messages

logLevelToHandle :: LogLevel -> Handle
logLevelToHandle a = case a of
  Info -> stdout
  Error -> stderr
  Regular -> stdout

logMessage :: MonadIO m => LogLevel -> Text -> m ()
logMessage level msg = liftIO . TIO.hPutStrLn (logLevelToHandle level) $ msg

-- Fatal error message, log and exit
logErrorAndExit :: MonadIO m => Text -> m b
logErrorAndExit msg = do
  logMessage Error $ "Error: " <> msg
  liftIO exitFailure
  -- We could choose different error codes depending on types of errors if
  -- that made sense for the application.

-- Musings:
--
-- For a *real* system we would want to build a logging abstraction that
-- lets us turn on logging, change log levels and direct messages to various
-- outputs (console out, AWS CloudWatch etc)
