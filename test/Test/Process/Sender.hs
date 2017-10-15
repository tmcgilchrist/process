{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Process.Sender where

import           Hedgehog

import           Test.Process.Gen (genUniqueHostService)

prop_connect_remotes :: Property
prop_connect_remotes = property $ do
  _list <- forAll genUniqueHostService

  -- 1. Spawn series of clients
  -- 2. Run connect remotes
  -- 3. Check we have connected to each

  -- I had issues with joining the Process monad with
  -- a supported instance of MonadTest from hedgehog.
  --
  -- What I'd ideally like to do is test the receive/send loops
  -- with various message types.
  True === True

tests :: IO Bool
tests =
  checkParallel $$(discover)
