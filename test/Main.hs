{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main
    ( main
    ) where

import Control.Monad (foldM, unless, void)
import Control.Monad.Writer (MonadIO (..), WriterT (..))
import Data.ByteArray qualified as B
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Foldable (for_)
import Data.Map.Strict qualified as M
import Data.MMR.InMemory
    ( MMR
    , Status (Closed, Open)
    , add
    , close
    , delete
    , lefts
    , mkMMR
    , open
    , orphans
    , proof
    , rights
    , verify
    )
import Data.MMR.SQL (Changes)
import Data.MMR.Types (mkH)
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Word (Word8)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , Property
    , Testable (property)
    , choose
    , counterexample
    , cover
    , elements
    , forAll
    , getSize
    , listOf
    , listOf1
    , resize
    , scale
    , vectorOf
    )
import Text.Pretty.Simple (pPrint, pShow)

chars :: (Word8, Word8)
chars = (fromIntegral $ ord 'a', fromIntegral $ ord 'z')

allWord8 :: (Word8, Word8)
allWord8 = (0, 255)

messageGen :: B.ByteArray b => (Word8, Word8) -> Gen b
messageGen cs = do
    s <- getSize
    B.pack <$> vectorOf s (choose cs)

inclusion :: ByteString -> MMR Closed -> Bool
inclusion msg mmr = verify msg (proof msg mmr) mmr

type W = WriterT (Seq Changes) IO

runW :: WriterT w m a -> m (a, w)
runW = runWriterT

evalW :: Functor m => WriterT w m a -> m a
evalW = fmap fst . runW

mkMMR' :: [ByteString] -> W (MMR Open)
mkMMR' = expand mkMMR

expand :: MMR Open -> [ByteString] -> W (MMR Open)
expand = foldM (flip add)

forAllMessages'
    :: (Show a, Testable prop, B.ByteArray b)
    => (Gen b -> Gen a)
    -> (a -> prop)
    -> Property
forAllMessages' l f =
    forAll (elements [chars, allWord8])
        $ \cs -> forAll (g cs) f
  where
    g cs =
        scale (* 25)
            $ l (messageGen cs)

forAllMessageBlocks
    :: Testable p => ([ByteString] -> p) -> Property
forAllMessageBlocks = forAllMessages' listOf1

forAllMessages :: Testable p => (ByteString -> p) -> Property
forAllMessages = forAllMessages' id

counterexampleT :: Testable prop => Text -> prop -> Property
counterexampleT msg = counterexample (T.unpack msg)

main :: IO ()
main = hspec $ do
    describe "An MMR" $ do
        it "contains symmetric up and down mappings"
            $ forAllMessageBlocks
            $ \msgs -> do
                mmr <- evalW $ mkMMR' msgs
                let roundTrips first second =
                        for_ (M.assocs first)
                            $ \(k, v) -> do
                                let v' = M.lookup v second
                                Just k `shouldBe` v'
                roundTrips (rights mmr) (lefts mmr)
                roundTrips (lefts mmr) (rights mmr)
        it "contains only one orphan when closed"
            $ forAllMessageBlocks
            $ \msgs -> do
                mmr <- evalW $ mkMMR' msgs >>= close
                length (orphans mmr) `shouldBe` 1
        it "can prove inclusion for any message in the MMR"
            $ forAllMessageBlocks
            $ \msgs -> do
                (mmr, _) <- runW $ mkMMR' msgs >>= close
                let included = Set.fromList msgs
                for_ msgs $ \msg -> do
                    inclusion msg mmr `shouldBe` True
        it "cannot prove inclusion for any message not in the MMR"
            $ forAllMessageBlocks
            $ \msgs -> forAllMessages $ \msg -> do
                mmr <- evalW $ mkMMR' msgs >>= close
                let included = Set.fromList msgs
                unless (Set.member msg included) $ do
                    inclusion msg mmr `shouldBe` False
        it "can change between open and close seamlessly"
            $ forAllMessageBlocks
            $ \msgs -> evalW $ do
                open0 <- mkMMR' msgs
                close0 <- close open0
                open1 <- open close0
                liftIO $ open0 `shouldBe` open1
        it "can be expanded with new messages after close and open"
            $ forAllMessageBlocks
            $ \msgs ->
                forAllMessageBlocks $ \newMsgs -> evalW $ do
                    open0 <- mkMMR' msgs
                    close0 <- close open0
                    open1 <- open close0
                    open2 <- expand open1 newMsgs
                    close1 <- close open2
                    liftIO $ for_ (msgs <> newMsgs) $ \msg ->
                        inclusion msg close1 `shouldBe` True
        it "cannot prove inclusion of deleted messages"
            $ forAllMessageBlocks
            $ \msgs -> forAll (elements msgs)
                $ \msg -> evalW $ do
                    open0 <- mkMMR' msgs
                    open1 <- delete msg open0
                    close1 <- close open1
                    liftIO $ inclusion msg close1 `shouldBe` False
        it "can still prove inclusion of other messages after deletion"
            $ forAllMessageBlocks
            $ \msgs -> forAll (elements msgs)
                $ \msg -> evalW $ do
                    open0 <- mkMMR' msgs
                    open1 <- delete msg open0
                    close1 <- close open1
                    liftIO $ for_ (Set.delete msg (Set.fromList msgs)) $ \m ->
                        inclusion m close1 `shouldBe` True
