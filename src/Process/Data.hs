{-# LANGUAGE DeriveGeneric #-}
module Process.Data (
  -- CLI Data Types

    SendFor (..)
  , WaitFor (..)
  , WithSeed (..)
  , Port (..)
  , Host (..)

  -- Core Data Types
  , Msg (..)
  , Payload (..)
  , Timestamp (..)
  , Timeout (..)
  ) where

import           Data.Typeable (Typeable)
import           Data.Binary (Binary)

import           GHC.Generics (Generic)

import           Control.Distributed.Process (ProcessId(..))

----------------------------------------------------------------------------
-- CLI Data types
--
newtype SendFor = SendFor { unSendFor :: Int } deriving (Eq, Show)
newtype WaitFor = WaitFor { unWaitFor :: Int } deriving (Eq, Show)

newtype Port = Port { unPort :: String } deriving (Eq, Show)
newtype Host = Host { unHost :: String } deriving (Eq, Show)

newtype WithSeed
  = WithSeed { unWithSeed :: Int }
  deriving (Eq, Show)

----------------------------------------------------------------------------
-- Core Data types
--

-- Wrapper message type for sending across the wire.
data Msg = Msg {
    mPayload :: Payload
  , mTimestamp :: Timestamp
  } deriving (Show, Generic, Typeable)

instance Binary Msg

-- One type of payload for a messages to send.
data Payload = Payload {
    pProcessId :: ProcessId
  , pSequence :: Int
  , pRandom :: Double
  } deriving (Show, Generic, Typeable)

instance Binary Payload

newtype Timestamp = Timestamp {
    unTimestamp :: Int
  } deriving (Show, Generic, Typeable, Ord, Eq)

instance Binary Timestamp

data Timeout = Timeout
  deriving (Show, Generic, Typeable)

instance Binary Timeout

-- Musings:
-- What about making msg parameterised by `a` and then threading `Payload ` or
-- `Timeout` through it? Would having functor / foldable / traversable buy us
-- any additional things?
--
-- We might be able to build a better / more efficient serialisation of these types
-- if we hand wrote the Binary instances. Not worth the effort without some more
-- time and measurements.
--
-- Also it might be nice to version messages, if we were building an application on
-- top of the `Payload` data type.
--
-- Another thing to consider is whether to make the data types more strict and inline things
-- Again something to measure and change if required, we don't want to optimise something
-- that doesn't need it.
