{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
    ( SomeException
    , handle
    )
import Data.MMR.InMemory.Oracle (newOracle)
import Data.MMR.InMemory.User (newUser, verify)
import Data.MMR.Interface (E, Oracle (..), User (..))
import Data.MMR.Types (Hash, Proof)
import Data.String (IsString (..))

data Interaction = Interaction
    { add :: String -> IO ()
    , rm :: String -> IO ()
    , prf :: String -> IO Proof
    , rt :: IO Hash
    , ver :: String -> Proof -> Hash -> IO Bool
    , sh :: IO ()
    }

reportLeft :: IO (E a) -> IO a
reportLeft m = do
    r <- m
    case r of
        Left e -> error $ Prelude.show e
        Right a -> pure a

newInteraction :: IO Interaction
newInteraction = do
    oracle <- newOracle
    user <- newUser
    let add x = do
            cs <- reportLeft $ insert oracle (fromString x)
            update user cs
        rm x = do
            cs <- reportLeft $ delete oracle (fromString x)
            update user cs
        prf = proof user . fromString
        rt = do
            cs <- reportLeft (close oracle)
            update user cs
            let reopen = do
                    cs' <- reportLeft (open oracle)
                    update user cs'
            handle (\(e :: SomeException) -> reopen >> error (Prelude.show e)) $ do
                (_l, h) <- reportLeft $ root oracle
                reopen
                pure h
        ver x prf' rt' = pure $ verify (fromString x) prf' rt'
        sh = do
            (rights, lefts) <- reportLeft $ serializeOracle oracle
            putStrLn $ "Oracle: " ++ Prelude.show (rights, lefts)
            (rights', lefts') <- reportLeft $ serializeUser user
            putStrLn $ "User: " ++ Prelude.show (rights', lefts')
    pure Interaction{..}

main :: IO ()
main = pure ()
