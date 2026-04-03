{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.InterfaceSpec
    ( interfaceSpecs
    ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.State.Strict (MonadTrans (..))
import Control.Monad.Writer
    ( MonadWriter (..)
    , WriterT (..)
    )
import Data.ByteArray qualified as B
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Foldable (for_)
import Data.MMR.InMemory.User.Core
    ( verify
    )
import Data.MMR.Interface
    ( Oracle (..)
    , OracleError
    , User (..)
    )
import Data.MMR.Types (Change (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Word (Word8)
import Test.Hspec
    ( SpecWith
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , NonEmptyList (..)
    , choose
    , elements
    , forAll
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

interfaceSpecs
    :: SpecWith (IO (Oracle IO ByteString), IO (User IO ByteString))
interfaceSpecs = do
    describe "Oracle interface and User interface" $ do
        it "can prove inclusions of all facts of one day"
            $ \(newOracle, newUser) -> forAll arbitrary25
                $ \(NonEmpty (Facts facts)) -> do
                    Oracle{insert, close, root} <- newOracle
                    User{update, proof} <- newUser
                    (rootHash, updates) <- runOracle $ do
                        for_ facts $ t . insert
                        t close
                        l root
                    update updates
                    for_ facts $ \msg -> do
                        p <- proof msg
                        verify msg p rootHash `shouldBe` True

        it "can prove inclusions of all facts of two days"
            $ \(newOracle, newUser) -> forAll arbitrary25
                $ \(Facts factsOfDay1) -> forAll arbitrary25
                    $ \(NonEmpty (Facts factsOfDay2)) -> do
                        Oracle{insert, close, open, root} <- newOracle
                        User{update, proof} <- newUser
                        (_, updatesDay1) <- runOracle $ do
                            for_ factsOfDay1 $ t . insert
                            t close
                        update updatesDay1
                        (rootDay2, updatesDay2) <- runOracle $ do
                            t open
                            for_ factsOfDay2 $ t . insert
                            t close
                            l root
                        update updatesDay2
                        for_ (factsOfDay1 <> factsOfDay2) $ \msg -> do
                            p <- proof msg
                            verify msg p rootDay2 `shouldBe` True

        it
            "can prove exclusion of facts which were not fed into the oracle"
            $ \(newOracle, newUser) -> forAll arbitrary25
                $ \(NonEmpty (Facts facts)) -> forAll arbitrary25
                    $ \(Facts (nonFacts :: [ByteString])) -> do
                        Oracle{insert, close, root} <- newOracle
                        User{update, proof} <- newUser
                        (rootHash, updates) <- runOracle $ do
                            for_ facts $ t . insert
                            t close
                            l root
                        update updates
                        let included = Set.fromList facts
                        for_ @[] nonFacts $ \msg -> do
                            p <- proof msg
                            unless (Set.member msg included) $ do
                                verify msg p rootHash `shouldBe` False

        it "can prove exclusion of deleted facts in the same day"
            $ \(newOracle, newUser) -> forAll arbitrary25
                $ \(NonEmpty (Facts facts), Fact fact) -> do
                    let allFacts = fact : facts
                    forAll (elements allFacts)
                        $ \toBeDeletedFact -> do
                            Oracle{insert, delete, close, root} <- newOracle
                            User{update, proof} <- newUser
                            (rootHash, updates) <- runOracle $ do
                                for_ allFacts $ t . insert
                                t $ delete toBeDeletedFact
                                t close
                                l root
                            update updates
                            let included = Set.fromList allFacts
                            for_ (Set.delete toBeDeletedFact included)
                                $ \msg -> do
                                    p <- proof msg
                                    unless (Set.member msg included) $ do
                                        verify msg p rootHash `shouldBe` True

        it "can still prove inclusion of other facts after deletion of one"
            $ \(newOracle, newUser) -> forAll arbitrary25
                $ \(NonEmpty (Facts facts), Fact fact) -> do
                    let allFacts = fact : facts
                    forAll (elements allFacts)
                        $ \toBeDeletedFact -> do
                            Oracle{insert, delete, close, root} <- newOracle
                            User{update, proof} <- newUser
                            (rootHash, updates) <- runOracle $ do
                                for_ allFacts $ t . insert
                                t $ delete toBeDeletedFact
                                t close
                                l root
                            update updates
                            let included = Set.fromList allFacts
                            for_ (Set.delete toBeDeletedFact included)
                                $ \msg -> do
                                    p <- proof msg
                                    verify msg p rootHash `shouldBe` True

l
    :: IO (Either OracleError x)
    -> WriterT [Change] (ExceptT OracleError IO) x
l f = lift (liftEither =<< lift f)

t
    :: IO (Either OracleError [Change])
    -> WriterT [Change] (ExceptT OracleError IO) ()
t f = l f >>= tell

runOracle :: WriterT w (ExceptT OracleError IO) a -> IO (a, w)
runOracle = fmap right . runExceptT . runWriterT

tellRight :: Show e => (Seq a -> b) -> Either e [a] -> b
tellRight t' (Right x) = t' $ Seq.fromList x
tellRight _ (Left e) = error $ "Expected Right " ++ show e

right :: Show e => Either e a -> a
right (Right x) = x
right (Left e) = error $ "Expected Right " ++ show e
