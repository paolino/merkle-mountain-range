{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Test.Generate
where

import Data.ByteArray qualified as B
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Word (Word8)
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , choose
    , getSize
    , scale
    , vectorOf
    )

chars :: (Word8, Word8)
chars = (fromIntegral $ ord 'a', fromIntegral $ ord 'z')

allWord8 :: (Word8, Word8)
allWord8 = (0, 255)

messageGen :: B.ByteArray b => (Word8, Word8) -> Gen b
messageGen cs = do
    s <- getSize
    B.pack <$> vectorOf s (choose cs)

newtype Fact = Fact {factOf :: ByteString}
    deriving (Show, Eq)

instance Arbitrary Fact where
    arbitrary = Fact <$> messageGen chars

pattern Facts :: Functor f => f ByteString -> f Fact
pattern Facts xs <- (fmap factOf -> xs)

arbitrary25 :: Arbitrary a => Gen a
arbitrary25 = scale (* 25) arbitrary
