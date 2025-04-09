{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.InMemory.CoreSpec
    ( coreSpecs
    ) where

import Control.Monad (foldM, void)
import Control.Monad.State.Strict (StateT (..))
import Control.Monad.Writer
    ( MonadIO (..)
    , WriterT (..)
    )
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..), for_)
import Data.MMR.InMemory.Oracle.Core
    ( MMR
    , Status (Open)
    , add
    , emptyOracle
    , orphans
    , seal
    , unseal
    )
import Data.MMR.InMemory.Oracle.Core qualified as O
import Data.MMR.InMemory.User.Core
    ( Proofs (..)
    , emptyProofs
    , expand
    )
import Data.MMR.InMemory.User.Core qualified as U
import Data.MMR.Types (Change (..))
import Data.Map.Strict qualified as M
import Data.Sequence (Seq)
import Test.Generate (arbitrary25, pattern Facts)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( forAll
    )

type OracleM = WriterT (Seq Change) IO

runOracle :: WriterT w m a -> m (a, w)
runOracle = runWriterT

feedOracle :: [ByteString] -> MMR Open -> OracleM (MMR Open)
feedOracle xs s = foldM (flip add) s xs

feedUser :: Seq Change -> StateT Proofs IO ()
feedUser = expand . toList

runUser :: b -> StateT b m a -> m (a, b)
runUser = flip runStateT

coreSpecs :: Spec
coreSpecs = do
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
