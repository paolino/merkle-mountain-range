{-# LANGUAGE RankNTypes #-}

module Data.MMR.Interface
where

import Data.MMR.Types (Change, Hash, Proof)
import Data.Map.Strict (Map)

data OracleError
    = OracleIsClosed
    | OracleIsOpen
    | OracleIsEmpty
    deriving (Eq, Show)

type E = Either OracleError

data Oracle m d = Oracle
    { insert :: d -> m (E [Change])
    , delete :: d -> m (E [Change])
    , close :: m (E [Change])
    , open :: m (E [Change])
    , root :: m (E Hash)
    , serializeOracle :: m (E (Map Hash Hash, Map Hash Hash))
    }

data Result m d = NoMoreResults | Result d (m (Result m d))

hoistResult
    :: (Functor n, Functor m)
    => (forall a. m a -> n a) -> Result m d -> Result n d
hoistResult _ NoMoreResults = NoMoreResults
hoistResult f (Result d m) = Result d (f $ fmap (hoistResult f) m)

data User m d = User
    { update :: [Change] -> m ()
    , proof :: d -> m Proof
    , search :: (d -> Bool) -> m (Result m d)
    , serializeUser :: m (Map Hash Hash, Map Hash Hash)
    }
