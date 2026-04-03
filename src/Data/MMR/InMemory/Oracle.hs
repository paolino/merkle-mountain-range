{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.MMR.InMemory.Oracle
    ( newOracle
    , mkOracle
    , Oracle (..)
    ) where

import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Control.Monad.Writer
    ( WriterT (..)
    )
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.MMR.InMemory.Oracle.Core
    ( MMR (..)
    , Status (Closed, Open)
    , add
    , emptyOracle
    , remove
    , seal
    , top
    , unseal
    )
import Data.MMR.Interface
    ( Oracle (..)
    , OracleError (OracleIsClosed, OracleIsEmpty, OracleIsOpen)
    )

type OracleState = Either (MMR Open) (MMR Closed)

type PureOracleMonad m = StateT OracleState m

type PureOracle m = Oracle (PureOracleMonad m) ByteString

mkOracle
    :: Monad m
    => PureOracle m
mkOracle = do
    Oracle
        { insert = insert'
        , delete = delete'
        , close = close'
        , open = open'
        , root = root'
        }
  where
    insert' v = do
        anMMR <- get
        case anMMR of
            Left openMMR -> do
                (openMMR', cs) <- runWriterT $ add v openMMR
                put $ Left openMMR'
                pure $ Right $ toList cs
            Right _ -> pure $ Left OracleIsClosed
    delete' v = do
        anMMR <- get
        case anMMR of
            Left openMMR -> do
                (openMMR', cs) <- runWriterT $ remove v openMMR
                put $ Left openMMR'
                pure $ Right $ toList cs
            Right _ -> pure $ Left OracleIsClosed
    close' = do
        anMMR <- get
        case anMMR of
            Left openMMR -> do
                (closedMMR, cs) <- runWriterT $ seal openMMR
                put $ Right closedMMR
                pure $ Right $ toList cs
            Right _ -> pure $ Left OracleIsClosed
    open' = do
        anMMR <- get
        case anMMR of
            Left _ -> pure $ Left OracleIsOpen
            Right closedMMR -> do
                (openMMR, cs) <- runWriterT $ unseal closedMMR
                put $ Left openMMR
                pure $ Right $ toList cs
    root' = do
        anMMR <- get
        case anMMR of
            Left _ -> pure $ Left OracleIsOpen
            Right closedMMR -> do
                case top closedMMR of
                    Nothing -> pure $ Left OracleIsEmpty
                    Just r -> pure $ Right r

withOracleState
    :: TVar OracleState -> PureOracleMonad STM a -> IO a
withOracleState q f = atomically $ do
    s <- readTVar q
    (r, s') <- runStateT f s
    writeTVar q s'
    pure r

newOracle :: IO (Oracle IO ByteString)
newOracle = do
    s <- newTVarIO $ Left emptyOracle
    let Oracle
            { insert = insert'
            , delete = delete'
            , close = close'
            , open = open'
            , root = root'
            } = mkOracle
    pure
        $ Oracle
            { insert = withOracleState s . insert'
            , delete = withOracleState s . delete'
            , close = withOracleState s close'
            , open = withOracleState s open'
            , root = withOracleState s root'
            }
