{-# LANGUAGE OverloadedStrings #-}
module Test.Process.Gen where

import qualified Data.List as DL
import           Data.Semigroup ((<>))
import           Data.String (IsString(..))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Network.Socket (HostName, ServiceName)

-- Generators for use in property testing.
-- No Arbitrary type classes here, after all we aren't savages.

genHostName :: Gen HostName
genHostName = Gen.element boats

genServiceName :: Gen ServiceName
genServiceName =
  (show :: Integer -> String)
  <$> Gen.integral (Range.constant 8000 9000)

genPair :: Gen (HostName, ServiceName)
genPair = do
  a <- genHostName
  b <- genServiceName
  pure (a,b)

genUniqueHostService :: Gen [(HostName, ServiceName)]
genUniqueHostService = do
  services <- Gen.list (Range.linear 1 50) genPair
  pure $ DL.zipWith (\(n,s) a -> (n, s <> show a)) services [1 .. length services]

waters :: IsString a => [a]
waters = [
    "basin"
  , "bay"
  , "billabong"
  , "canal"
  , "channel"
  , "creek"
  , "estuary"
  , "fjord"
  , "harbour"
  , "lake"
  , "loch"
  , "marsh"
  , "ocean"
  , "pond"
  , "puddle"
  , "reservoir"
  , "river"
  , "sea"
  , "slough"
  , "sound"
  , "spring"
  , "stream"
  , "swamp"
  , "wetland"
  ]

boats :: IsString a => [a]
boats = [
    "barge"
  , "battleship"
  , "canoe"
  , "catamaran"
  , "dinghy"
  , "ferry"
  , "gondola"
  , "jetski"
  , "kayak"
  , "longship"
  , "motorboat"
  , "pontoon"
  , "powerboat"
  , "rowboat"
  , "ship"
  , "steamboat"
  , "tanker"
  , "trawler"
  , "tugboat"
  , "yacht"
  ]
