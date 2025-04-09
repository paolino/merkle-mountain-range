{-# LANGUAGE FlexibleContexts #-}

module Data.MMR.InMemory.User
    ( newUser
    , PureUser
    , verify
    , mkUser
    , User (..)
    ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad.STM (STM, atomically)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.ByteString (ByteString)
import Data.MMR.InMemory.User.Core
    ( Proofs (..)
    , emptyProofs
    , expand
    , mkProof
    , verify
    )
import Data.MMR.Interface
    ( User (..)
    , hoistResult
    )

type PureUser m = User (StateT Proofs m) ByteString

mkUser :: Monad m => PureUser m
mkUser = do
    User
        { update = update'
        , proof = proof'
        , search = undefined
        , serializeUser = serializeUser'
        }
  where
    update' = expand
    proof' v = mkProof v <$> get
    serializeUser' = do
        ps <- get
        pure (rights ps, lefts ps)

withProofs
    :: TVar Proofs
    -> StateT Proofs STM a
    -> IO a
withProofs q f = atomically $ do
    ps <- readTVar q
    (a, ps') <- runStateT f ps
    writeTVar q ps'
    pure a

newUser :: IO (User IO ByteString)
newUser = do
    q <- newTVarIO emptyProofs
    let User
            { update = update'
            , proof = proof'
            , search = search'
            , serializeUser = serializeUser'
            } = mkUser
    pure
        $ User
            { update = withProofs q . update'
            , proof = withProofs q . proof'
            , search = \b -> do
                r <- withProofs q $ search' b
                pure $ hoistResult (withProofs q) r
            , serializeUser = withProofs q serializeUser'
            }
