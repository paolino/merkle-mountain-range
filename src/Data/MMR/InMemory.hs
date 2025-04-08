{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.MMR.InMemory
    ( -- * Types
      Level
    , MMR (rights, lefts, orphans)
    , Proof
    , Combine
    , Status (Closed, Open)
    , Seam (oldOrphans, removeRights, removeLefts)

      -- * create an MMR
    , mkMMR
    , add
    , delete

      -- * closing and opening an MMR
    , close
    , open

      -- * query an MMR
    , root
    , proof
    , verify

      -- * test data
    ) where

import Control.Lens (Lens', (%=))
import Control.Monad (void, when)
import Control.Monad.State (MonadState (..), StateT, execStateT)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Control.Monad.Writer (MonadTrans (..), MonadWriter (..))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..), for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.MMR.SQL (Changes (..))
import Data.MMR.Types (Hash, Level, mkH)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

-- | Type level for the MMR status. ATM opening a closed MMR is not
-- supported.
data Status = Closed | Open

-- | A seam is a structure to hold the reverse operations to re-open a closed MMR.
-- MMR
data Seam = Seam
    { oldOrphans :: Map Level Hash
    -- ^ The full orphans that have to be restored
    , removeRights :: Set Hash
    -- ^ The hashes that have to be removed from the rights map
    , removeLefts :: Set Hash
    -- ^ The hashes that have to be removed from the lefts map
    }
    deriving (Show, Eq)

emptySeam :: Seam
emptySeam = Seam mempty mempty mempty

--
type family OnClosed (a :: Status) where
    OnClosed 'Closed = Seam
    OnClosed 'Open = ()

-- | A Merkle Mountain Range (MMR) is a data structure that allows for efficient
-- append-only storage of data, with the ability to prove the inclusion of
-- elements in the structure. The type can be found in both open and closed
-- states. The open state is used for adding new elements, while the closed
-- state is used for querying the structure.
-- The 2 state are isomorphic, so we can go from one to the other via 'close'
-- and 'open'.
data MMR (a :: Status) = MMR
    { rights :: Map Hash Hash
    , lefts :: Map Hash Hash
    , orphans :: Map Level Hash
    , seam :: OnClosed a
    }

deriving instance (Show (OnClosed a)) => Show (MMR a)
deriving instance (Eq (OnClosed a)) => Eq (MMR a)

rightsL :: Lens' (MMR a) (Map Hash Hash)
rightsL f mmr@MMR{rights} = (\rights' -> mmr{rights = rights'}) <$> f rights

leftsL :: Lens' (MMR a) (Map Hash Hash)
leftsL f mmr@MMR{lefts} = (\lefts' -> mmr{lefts = lefts'}) <$> f lefts

orphansL :: Lens' (MMR a) (Map Level Hash)
orphansL f mmr@MMR{orphans} = (\orphans' -> mmr{orphans = orphans'}) <$> f orphans

insertRight
    :: MonadWriter (Seq Changes) m => Hash -> Hash -> StateT (MMR a) m ()
insertRight h h' = do
    push $ InsertRight h h'
    rightsL %= M.insert h h'

deleteRight
    :: MonadWriter (Seq Changes) m => Hash -> StateT (MMR a) m ()
deleteRight h = do
    push $ DeleteRight h
    rightsL %= M.delete h

insertLeft
    :: MonadWriter (Seq Changes) m => Hash -> Hash -> StateT (MMR a) m ()
insertLeft h h' = do
    push $ InsertLeft h h'
    leftsL %= M.insert h h'

deleteLeft
    :: MonadWriter (Seq Changes) m => Hash -> StateT (MMR a) m ()
deleteLeft h = do
    push $ DeleteLeft h
    leftsL %= M.delete h

insertOrphan
    :: MonadWriter (Seq Changes) m => Level -> Hash -> StateT (MMR a) m ()
insertOrphan l h = orphansL %= M.insert l h

deleteOrphan
    :: MonadWriter (Seq Changes) m => Level -> StateT (MMR a) m ()
deleteOrphan l = orphansL %= M.delete l

push :: MonadWriter (Seq Changes) m => Changes -> m ()
push = tell . Seq.singleton

addH
    :: MonadWriter (Seq Changes) m
    => Level
    -> Hash
    -> StateT (MMR Open) m ()
addH l h = do
    MMR{rights, lefts, orphans} <- get
    case M.lookup l orphans of
        Nothing ->
            when (M.notMember h rights && M.notMember h lefts) $ do
                insertOrphan l h
        Just h' -> do
            deleteOrphan l
            insertRight h' h
            insertLeft h h'
            addH (l + 1) (h' <> h)

-- | Add a new element to the MMR. The element is hashed and stored in the
-- MMR
add
    :: MonadWriter (Seq Changes) m => ByteString -> MMR Open -> m (MMR Open)
add v mmr = flip execStateT mmr $ addH 0 $ mkH v

-- | Delete an element from the MMR. The element is hashed and removed from the
-- MMR.
delete
    :: MonadWriter (Seq Changes) m => ByteString -> MMR Open -> m (MMR Open)
delete v mmr = flip execStateT mmr $ climb 0 (mkH v) >>= mapM_ (uncurry addH)

climb
    :: (MonadWriter (Seq Changes) m)
    => Level
    -> Hash
    -> StateT (MMR Open) m [(Level, Hash)]
climb l h = do
    MMR{rights, lefts} <- get
    case M.lookup h rights of
        Nothing -> case M.lookup h lefts of
            Nothing -> do
                deleteOrphan l
                pure []
            Just h' -> do
                deleteLeft h
                deleteRight h'
                rest <- climb (l + 1) (h' <> h)
                pure $ (l, h') : rest
        Just h' -> do
            deleteRight h
            deleteLeft h'
            rest <- climb (l + 1) (h <> h')
            pure $ (l, h') : rest

-- | A proof step
data Combine = Prepend Hash | Append Hash
    deriving (Show)

combine :: Combine -> Hash -> Hash
combine (Prepend h) h' = h <> h'
combine (Append h) h' = h' <> h

-- | A proof is a list of steps to combine hashes to get the root hash of the MMR
type Proof = [Combine]

applyProof :: Proof -> Hash -> Hash
applyProof ps h = foldl' (flip combine) h ps

proofH :: Hash -> MMR Closed -> Proof
proofH h mmr@MMR{rights, lefts} =
    case M.lookup h rights of
        Nothing -> case M.lookup h lefts of
            Nothing -> []
            Just h' -> Prepend h' : proofH (h' <> h) mmr
        Just h' -> Append h' : proofH (h <> h') mmr

-- | Get the proof for a given hash in the MMR. The proof is a list of steps to
proof :: ByteString -> MMR Closed -> Proof
proof = proofH . mkH

-- | Get the root hash of the MMR. The root hash is the hash of the entire MMR
root :: MMR Closed -> Maybe (Level, Hash)
root MMR{orphans} = M.lookupMax orphans

verifyH :: Hash -> [Combine] -> MMR Closed -> Bool
verifyH h path mmr = case root mmr of
    Nothing -> False
    Just (_l, h') -> applyProof path h == h'

-- | Verify a proof for a given hash in the MMR. The proof is a list of steps to
verify :: ByteString -> Proof -> MMR Closed -> Bool
verify = verifyH . mkH

-- | Create a new MMR from a list of facts (ordered by insertion). The MMR is
-- created by hashing each fact and adding it to the MMR.
mkMMR :: MMR Open
mkMMR = MMR mempty mempty mempty ()

seamL :: Lens' (MMR Closed) Seam
seamL f mmr@MMR{seam} = (\seam' -> mmr{seam = seam'}) <$> f seam

removeRighsL :: Lens' Seam (Set Hash)
removeRighsL f seam@Seam{removeRights} =
    (\removeRights' -> seam{removeRights = removeRights'})
        <$> f removeRights

removeLeftsL :: Lens' Seam (Set Hash)
removeLeftsL f seam@Seam{removeLefts} =
    (\removeLefts' -> seam{removeLefts = removeLefts'}) <$> f removeLefts

closing
    :: MonadWriter (Seq Changes) m => MaybeT (StateT (MMR Closed) m) ()
closing = do
    MMR{orphans} <- get
    (l, r) <- hoistMaybe $ M.lookupMin orphans
    (l', r') <- hoistMaybe $ M.lookupMin $ M.delete l orphans
    lift $ do
        seamL . removeRighsL %= Set.insert r'
        seamL . removeLeftsL %= Set.insert r
        deleteOrphan l
        deleteOrphan l'
        insertRight r' r
        insertLeft r r'
        insertOrphan (l + 1) (r' <> r)
    closing

-- | Seam the MMR by removing the orphan nodes and making sure all nodes are
-- connected. Also store the reverse operation in the seam field.
close :: MonadWriter (Seq Changes) m => MMR Open -> m (MMR Closed)
close MMR{rights, lefts, orphans} =
    execStateT (void $ runMaybeT closing)
        $ MMR rights lefts orphans emptySeam{oldOrphans = orphans}

-- | Unseam the MMR by restoring the orphan nodes. Apply and destroy the seam
-- field. This is the reverse operation of 'seamup'.
open :: MonadWriter (Seq Changes) m => MMR Closed -> m (MMR Open)
open
    MMR
        { rights
        , lefts
        , orphans
        , seam = Seam{oldOrphans, removeLefts, removeRights}
        } = flip execStateT MMR{rights, lefts, orphans, seam = ()}
        $ do
            for_ (Set.toList removeLefts) deleteLeft
            for_ (Set.toList removeRights) deleteRight
            for_ (M.keys orphans) deleteOrphan
            for_ (M.assocs oldOrphans) $ uncurry insertOrphan
