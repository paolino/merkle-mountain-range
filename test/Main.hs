{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main
    ( main
    ) where

import Control.Monad (foldM, foldM_, unless, void)
import Control.Monad.Cont (ContT (..), cont, runCont)
import Control.Monad.State.Strict (StateT (..))
import Control.Monad.Writer (MonadIO (..), WriterT (..))
import Data.ByteArray qualified as B
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Foldable (Foldable (..), for_)
import Data.MMR.InMemory.Oracle.Core
    ( MMR
    , Status (Closed, Open)
    , add
    , emptyOracle
    , lefts
    , orphans
    , remove
    , rights
    , seal
    , top
    , unseal
    )
import Data.MMR.InMemory.Oracle.Core qualified as O
import Data.MMR.InMemory.User.Core
    ( Proofs (..)
    , emptyProofs
    , expand
    , mkProof
    , verify
    )
import Data.MMR.InMemory.User.Core qualified as U
import Data.MMR.Types (Change (..), Hash, mkH)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Word (Word8)
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , NonEmptyList (..)
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

type OracleM = WriterT (Seq Change) IO

runOracle :: WriterT w m a -> m (a, w)
runOracle = runWriterT

feedOracle :: [ByteString] -> MMR Open -> OracleM (MMR Open)
feedOracle xs s = foldM (flip add) s xs

feedUser :: Seq Change -> StateT Proofs IO ()
feedUser = expand . toList

runUser :: b -> StateT b m a -> m (a, b)
runUser = flip runStateT

inclusion :: ByteString -> Proofs -> Maybe Hash -> Bool
inclusion msg mt (Just r) = verify msg (mkProof msg mt) r
inclusion _ _ Nothing = False

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
    arbitrary = Fact <$> messageGen allWord8 -- chars

pattern Facts :: Functor f => f ByteString -> f Fact
pattern Facts xs <- (fmap factOf -> xs)

arbitrary25 :: Arbitrary a => Gen a
arbitrary25 = scale (* 25) arbitrary

main :: IO ()
main = hspec $ do
    describe "An user-oracle system" $ do
        it
            "contains symmetric up and down mappings in user and oracle systems"
            $ forAll arbitrary25
            $ \(Facts facts) -> do
                (closed, update) <-
                    runOracle $ feedOracle facts emptyOracle >>= seal
                let roundTrips first second =
                        for_ (M.assocs first)
                            $ \(k, v) -> Just k `shouldBe` M.lookup v second
                roundTrips (O.rights closed) (O.lefts closed)
                roundTrips (O.lefts closed) (O.rights closed)
                (_, proofs) <- runUser emptyProofs $ feedUser update
                roundTrips (U.rights proofs) (U.lefts proofs)
                roundTrips (U.lefts proofs) (U.rights proofs)

        it "keep the same merkle tree in both user and oracle systems"
            $ forAll arbitrary25
            $ \(Facts facts) -> do
                (closed, updates) <-
                    runOracle
                        $ feedOracle facts emptyOracle >>= seal
                ((), proofs) <- runUser emptyProofs $ feedUser updates
                O.rights closed `shouldBe` U.rights proofs
                O.lefts closed `shouldBe` U.lefts proofs

        it "contains only one orphan in the oracle when closed"
            $ forAll arbitrary25
            $ \(Facts facts) -> do
                (closed, _) <- runOracle $ feedOracle facts emptyOracle >>= seal
                case facts of
                    [] -> length (orphans closed) `shouldBe` 0
                    _ -> length (orphans closed) `shouldBe` 1

        it "can change the oracle between sealed and unsealed"
            $ forAll arbitrary25
            $ \(Facts facts) -> void $ runOracle $ do
                fedup <- feedOracle facts emptyOracle
                sealed <- seal fedup
                unsealed <- unseal sealed
                liftIO $ fedup `shouldBe` unsealed

        it "can prove inclusions of all facts of one day"
            $ forAll arbitrary25
            $ \(Facts facts) -> do
                ((mroot, sealed), updates) <- runOracle $ do
                    fedup <- feedOracle facts emptyOracle
                    sealed <- seal fedup
                    pure (top sealed, sealed)
                ((), proofs) <- runUser emptyProofs $ feedUser updates
                for_ facts $ \msg -> do
                    inclusion msg proofs mroot `shouldBe` True

        it "can prove inclusions of all facts of two days"
            $ forAll arbitrary25
            $ \(Facts factsOfDay1) -> forAll arbitrary25
                $ \(Facts factsOfDay2) -> do
                    ((mrootDay1, sealedDay1), updatesDay1) <- runOracle $ do
                        fed <- feedOracle factsOfDay1 emptyOracle
                        sealed <- seal fed
                        pure (top sealed, sealed)
                    ((), proofsDay1) <-
                        runUser emptyProofs
                            $ feedUser updatesDay1
                    (mrootDay2, updatesDay2) <- runOracle $ do
                        unsealed <- unseal sealedDay1
                        fed <- feedOracle factsOfDay2 unsealed
                        sealed <- seal fed
                        pure (top sealed)
                    ((), proofsDay2) <-
                        runUser proofsDay1
                            $ feedUser updatesDay2
                    for_ (factsOfDay1 <> factsOfDay2) $ \msg -> do
                        inclusion msg proofsDay2 mrootDay2 `shouldBe` True

        it
            "can prove exclusion of facts which were not fed into the oracle"
            $ forAll arbitrary25
            $ \(Facts facts) -> forAll arbitrary25
                $ \(Facts nonFacts) -> do
                    ((mroot, _), updates) <- runOracle $ do
                        let unsealed = emptyOracle
                        fedup <- feedOracle facts unsealed
                        sealed <- seal fedup
                        pure (top sealed, sealed)
                    ((), proofs) <- runUser emptyProofs $ feedUser updates
                    let included = Set.fromList facts
                    for_ @[] nonFacts $ \msg -> do
                        unless (Set.member msg included) $ do
                            inclusion msg proofs mroot `shouldBe` False

        it "can prove exclusion of deleted facts in the same day"
            $ forAll arbitrary25
            $ \(NonEmpty (Facts facts)) -> forAll (elements facts)
                $ \toBeDeletedFact -> do
                    ((mroot, sealed), updates) <- runOracle $ do
                        fed <- feedOracle facts emptyOracle
                        pruned <- remove toBeDeletedFact fed
                        sealed <- seal pruned
                        pure (top sealed, sealed)
                    ((), proofs) <- runUser emptyProofs $ feedUser updates
                    inclusion toBeDeletedFact proofs mroot `shouldBe` False

        it "can still prove inclusion of other facts after deletion of one"
            $ forAll arbitrary25
            $ \(NonEmpty (Facts facts)) -> forAll (elements facts)
                $ \toBeDeletedFact -> do
                    ((mroot, sealed), updates) <- runOracle $ do
                        fed <- feedOracle facts emptyOracle
                        pruned <- remove toBeDeletedFact fed
                        sealed <- seal pruned
                        pure (top sealed, sealed)
                    ((), proofs) <- runUser emptyProofs $ feedUser updates
                    let included = Set.fromList facts
                    for_ (Set.delete toBeDeletedFact included) $ \msg -> do
                        inclusion msg proofs mroot `shouldBe` True
