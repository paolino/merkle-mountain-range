module Main
    ( main
    ) where

import Test.InMemorySpec (inMemorySpecs)

main :: IO ()
main = do
    inMemorySpecs
