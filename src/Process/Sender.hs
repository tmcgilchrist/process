{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Process.Sender (
  -- Main entry point for the cli
    startProcess

  -- Exposing with the view to property testing
  , connectRemotes
  , broadcastMessages
  , receiveMessages
  , runReceiver
  ) where

import           Control.Concurrent (MVar, putMVar, takeMVar, newEmptyMVar)
import           Control.Distributed.Process (Process, ProcessId(..), WhereIsReply(..), NodeId(..)
                                             , match, receiveWait, send, register, getSelfPid
                                             , expectTimeout, expect, whereisRemoteAsync, )
import           Control.Distributed.Process.Node (newLocalNode, initRemoteTable, forkProcess)
import           Control.Exception (IOException)
import           Control.Monad (forM_, void)
import           Control.Monad.IO.Class (liftIO)

import           Data.Either (either)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
import           Data.Sequence (Seq, (|>), ViewR((:>)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Network.Socket (HostName, ServiceName)
import           Network.Transport.TCP (createTransport, defaultTCPParameters, encodeEndPointAddress)

import           Process.Data
import           Process.Logging (LogLevel(..), logMessage, logErrorAndExit)
import           Process.Time (microsecond, millisecond, currentTime, waitUntil)

import           System.Clock (TimeSpec(..))
import           System.Random
-- Note about imports, everything from an external library is imported explicitly while
-- things from this library are not. It's useful to know where things are getting pulled from.

startProcess :: Host -> Port -> SendFor -> WaitFor -> WithSeed -> [(HostName, ServiceName)]-> IO ()
startProcess (Host host) (Port port) (SendFor sendFor') (WaitFor waitFor') (WithSeed seed) remotes = do
  let buffer = 1000 -- TODO Buffer size, make it customisable

  -- It might be nice to generate random numbers as we go,
  -- rather than allocate this up front
  let nums = randoms $ mkStdGen seed

  let sendFor = TimeSpec (fromIntegral sendFor') 0
  let waitFor = TimeSpec (fromIntegral waitFor') 0

  t <- createTransport host port defaultTCPParameters >>=
    either (logErrorAndExit . renderIOException) pure

  node <- newLocalNode t initRemoteTable

  -- Used to get results from the broadcasting and receiving processes.
  logMessage Info "Setup MVars ..."
  broadcastResult <- newEmptyMVar :: IO (MVar Int)
  receiveResult <- newEmptyMVar :: IO (MVar (Double, (Int, Int)))

  -- Receiver process.
  logMessage Info "Setup receiver process"
  receiver <- forkProcess node $ runReceiver host port buffer receiveResult

  logMessage Info "Setup Broadcaster process"
  broadcaster <- forkProcess node $ runBroadcaster broadcastResult nums

  -- Main process.
  --
  -- Received the ProcessId of the remote nodes and
  -- relays then to the broadcaster.
  logMessage Info "Setup Main process"
  void . forkProcess node $ runMainProcess remotes broadcaster sendFor waitFor receiver

  broadcastedCount <- takeMVar broadcastResult
  (result, (receivedCount, droppedCount)) <- takeMVar receiveResult

  let droppedPercent = calculateDropped droppedCount receivedCount

  logMessage Info $ "Total sent messages: " <> render broadcastedCount <> "\n"
                 <> "Total received messages: " <> render receivedCount <> "\n"
                 <> "Total dropped messages:" <> render droppedCount
                 <> " (" <> render droppedPercent <> "%) messages."

  logMessage Regular $ render receivedCount <> " " <> render (round result :: Integer)

-- Broadcast payloads to the specified PIDs.
--
-- Continuously sends messages until either:
-- 1. a 'Timeout' message is received
-- 2. collection of payloads is exhausted
--
-- Returns the total number of messages sent.
broadcastMessages :: [ProcessId] -> [Payload] -> Process Int
broadcastMessages pids payloads =
  go payloads 0

  where
    go :: [Payload] -> Int -> Process Int
    go [] n = pure n

    go (p : ps) n = do
      -- The current POSIX time in microseconds is the message timestamp.
      t <- Timestamp . round . (* microsecond) <$> liftIO getPOSIXTime :: Process Timestamp
      -- Send message to all PIDs.
      forM_ pids (flip send (p, t))
      -- If we see a `Timeout` messages we are done, so exit
      mTimeout <- expectTimeout 0 :: Process (Maybe Timeout)
      case mTimeout of
        Nothing ->
          go ps (n + 1)
        Just _ ->
          pure n

-- | Receive messages and compute the result. The pure value is a tuple of
-- the result and the number of messages received and dropped.
receiveMessages :: Int -> Process (Double, (Int, Int))
receiveMessages bufsize =
  go Seq.empty (0, 0) (Timestamp 0) 0.0

  where
    go :: Seq Msg -- Message buffer
       -> (Int, Int) -- Count of messages received and dropped.
       -> Timestamp -- The earliest timestamp we can receive to keep ordering.
       -> Double
       -> Process (Double, (Int, Int))

    go buf stats _ acc | Seq.length buf == bufsize =
        go Seq.empty stats t result
      where
        (t, result) = computeResult buf acc

    -- Receive a message or a timeout.
    go buf (n, nd) earliest acc = do
        result <- receiveWait
            [ match (\(msg :: Msg) -> pure (Right msg))
            , match (\(timeout :: Timeout) -> pure (Left timeout))
            ]
        case result of
          -- If the message was sent earlier than the earliest, drop it, since
          -- the window to use it in our computation has passed.
          Right (Msg _ t) | t < earliest ->
            go buf (n, nd + 1) earliest acc

          -- The message was sent within our time window, add it to the buffer.
          Right msg ->
            go (buf |> msg) (n + 1, nd) earliest acc

          -- Timeout received, we're done. Compute the results from the current
          -- buffer and exit.
          Left _ ->
            pure (snd (computeResult buf acc), (n, nd))

-- Compute the new result based on the buffer and current result.
-- Returns the latest timestamp from the buffer and the new result.
computeResult :: Seq Msg -> Double -> (Timestamp, Double)
computeResult buf accum =
  (,) latest $ foldr f accum sortedBuf
  where
    -- The computation to perform on each received (index, number) pair.
    f :: Msg -> Double -> Double
    f (Msg (Payload _ i x) _) acc = acc + fromIntegral i * x

    -- The buffer sorted by timestamp.
    sortedBuf :: Seq Msg
    sortedBuf = Seq.unstableSortBy (comparing mTimestamp) buf

    -- The largest timestamp of the buffer, which becomes the earliest
    -- we can process to preserve ordering.
    _ :> (Msg _ latest) = Seq.viewr sortedBuf

-- | Convert tuples of Hostname, Service to ProcessIds
connectRemotes :: [(HostName, ServiceName)] -> Process [ProcessId]
connectRemotes =
  flip go []

  where
    go :: [(HostName, ServiceName)] -> [ProcessId] -> Process [ProcessId]
    go remotes@((host, port) : rest) pids = do
      let pname = processName host port
      whereisRemoteAsync (NodeId $ encodeEndPointAddress host port 0) pname
      result <- expectTimeout $ 10 * millisecond    -- This could be exported or configured in some way.
      case result of
        Just (WhereIsReply name (Just pid)) ->
          case name == pname of
            True ->
              go rest (pid : pids)
            False ->
              go remotes pids
        _ ->
          go remotes pids
    go [] pids =
      pure pids

runReceiver :: HostName -> ServiceName -> Int -> MVar (Double, (Int, Int)) -> Process ()
runReceiver host port buffer receiveResult = do
  register (processName host port) =<< getSelfPid
  result <- receiveMessages buffer
  liftIO $ putMVar receiveResult result

runBroadcaster :: MVar Int -> [Double] -> Process ()
runBroadcaster broadcastResult nums = do
  self <- getSelfPid
  pids <- expect :: Process [ProcessId]
  result <- broadcastMessages pids $
            zipWith3 Payload (repeat self) [1..] nums
            -- zipWith3 If one input list is short, excess elements of the longer lists are discarded.
            -- unfortunate choice of a *lossy* function. The use of infinite sequences for the
            -- first 2 arguments should save us if we are careful.
  liftIO $ putMVar broadcastResult result

runMainProcess :: [(HostName, ServiceName)] -> ProcessId -> TimeSpec -> TimeSpec -> ProcessId -> Process ()
runMainProcess remotes broadcaster sendFor waitFor receiver = do
  pids <- connectRemotes remotes
  send broadcaster pids
  started <- currentTime

  logMessage Info $ "Broadcasting messages"
  finished <- waitUntil (\t -> t - started >= sendFor) $ \now ->
    send broadcaster Timeout >> pure now

  logMessage Info $ "Waiting for grace period"
  waitUntil (\t -> t - finished >= waitFor) $
    const (send receiver Timeout)

-- Build a (hopefully) unique process name.
--
-- No real checks are made that this is unique beforehand
-- it will however fall apart when trying to start up the
-- corresponding nodes.
processName :: HostName -> ServiceName -> String
processName host port = "receiver:" ++ host ++ ":" ++ port

-- Calculate the number of dropped messages
calculateDropped :: (Integral a, Integral b) => a -> b -> Integer
calculateDropped countDropped countReceived =
  round (fromIntegral countDropped / fromIntegral countReceived * 100 :: Double)

-- Typically I would not use show unless I *really* have to,
-- my preference is to use Text for all things over String.
-- Data.Text is nearly always a better default.
renderIOException :: IOException -> Text
renderIOException = T.pack . show

render :: Show a => a -> Text
render = T.pack . show
