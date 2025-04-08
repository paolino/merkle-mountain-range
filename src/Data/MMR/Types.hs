{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Data.MMR.Types
    ( -- * Types
      Hash
    , mkH
    , Level

    ) where

import Crypto.Hash (SHA256, hash)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)

-- | A hash is a 256-bit value
newtype Hash = Hash ByteString
    deriving (Eq, Ord, Show)

instance Semigroup Hash where
    Hash a <> Hash b = mkH (a <> b)

mkH :: ByteString -> Hash
mkH = Hash . convertToBase Base16 . hash @ByteString @SHA256

-- | A level is a non-negative integer representing the depth of a node in the MMR
newtype Level = Level
    { getLevel :: Int
    }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)
