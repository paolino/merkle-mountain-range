module Data.MMR.SQL
    ( -- * Types
      Changes (..)
    )
where

import Data.MMR.Types (Hash)

data Changes
    = InsertRight Hash Hash
    | DeleteRight Hash
    | InsertLeft Hash Hash
    | DeleteLeft Hash
