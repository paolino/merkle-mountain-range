{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Test.InMemorySpec
    ( inMemorySpecs
    ) where

import Data.MMR.InMemory.Oracle (newOracle)
import Data.MMR.InMemory.User (newUser)
import Test.Hspec
    ( beforeAll
    , hspec
    )
import Test.InMemory.CoreSpec (coreSpecs)
import Test.InterfaceSpec (interfaceSpecs)

inMemorySpecs :: IO ()
inMemorySpecs = do
    hspec coreSpecs
    hspec $ beforeAll (pure (newOracle, newUser)) interfaceSpecs
