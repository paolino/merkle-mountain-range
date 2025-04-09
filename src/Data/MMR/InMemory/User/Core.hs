{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

module Data.MMR.InMemory.User.Core
    ( -- * Types
      Proof
    , Proofs (rights, lefts)
    , emptyProofs
    , Combine
    , expand
    , mkProof
    , verify
    ) where

import Control.Lens (Lens', (%=))
import Control.Monad (foldM_)
import Control.Monad.State.Strict (StateT (..))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.MMR.Types (Change (..), Combine (..), Hash, Proof, mkH)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

data Proofs = Proofs
    { rights :: Map Hash (Hash, Hash)
    , lefts :: Map Hash (Hash, Hash)
    }
    deriving (Show, Eq)

emptyProofs :: Proofs
emptyProofs = Proofs mempty mempty

rightsL :: Lens' Proofs (Map Hash (Hash, Hash))
rightsL f proofs@Proofs{rights} = (\rights' -> proofs{rights = rights'}) <$> f rights

leftsL :: Lens' Proofs (Map Hash (Hash, Hash))
leftsL f proofs@Proofs{lefts} = (\lefts' -> proofs{lefts = lefts'}) <$> f lefts

insert
    :: Monad m => Hash -> Hash -> Hash -> StateT Proofs m ()
insert h h' hh' = do
    rightsL %= M.insert h (h', hh')
    leftsL %= M.insert h' (h, hh')

deleteRight
    :: Monad m => Hash -> StateT Proofs m ()
deleteRight h = do
    rightsL %= M.delete h

deleteLeft
    :: Monad m => Hash -> StateT Proofs m ()
deleteLeft h = do
    leftsL %= M.delete h

expand :: Monad m => [Change] -> StateT Proofs m ()
expand = foldM_ go ()
  where
    go :: Monad m => () -> Change -> StateT Proofs m ()
    go _ (Insert h h' hh') = insert h h' hh'
    go _ (DeleteRight h) = deleteRight h
    go _ (DeleteLeft h) = deleteLeft h

combine :: Combine -> Hash -> Hash
combine (Prepend h) h' = h <> h'
combine (Append h) h' = h' <> h

-- | A proof is a list of steps to combine hashes to get the root hash of the Proofs
applyProof :: Proof -> Hash -> Hash
applyProof ps h = foldl' (flip combine) h ps

proofH :: Hash -> Proofs -> Proof
proofH h proofs@Proofs{rights, lefts} =
    case M.lookup h rights of
        Nothing -> case M.lookup h lefts of
            Nothing -> []
            Just (h', h'h) -> Prepend h' : proofH h'h proofs
        Just (h', hh') -> Append h' : proofH hh' proofs

-- | Get the proof for a given hash in the Proofs. The proof is a list of steps to
mkProof :: ByteString -> Proofs -> Proof
mkProof = proofH . mkH

-- | Verify a proof for a given hash in the Proofs. The proof is a list of steps to
verify :: ByteString -> Proof -> Hash -> Bool
verify msg prf rt
    | null prf = mkH msg == rt
    | otherwise = applyProof prf (mkH msg) == rt
