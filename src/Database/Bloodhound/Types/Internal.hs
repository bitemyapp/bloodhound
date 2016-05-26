{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-------------------------------------------------------------------------------
-- |
-- Module : Database.Bloodhound.Types.Internal
-- Copyright : (C) 2014 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com>
-- Stability : provisional
-- Portability : DeriveGeneric, RecordWildCards
--
-- Internal data types for Bloodhound. These types may change without
-- notice so import at your own risk.
-------------------------------------------------------------------------------
module Database.Bloodhound.Types.Internal
    ( BHEnv(..)
    , Server(..)
    , newServerNoSniff
    , newServer
    , defSniffFreq
    , MonadBH(..)
    ) where


import           Control.Applicative
import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad.Reader
import qualified Data.Aeson           as JSON
import qualified Data.HashMap.Strict  as HM
import qualified Data.RoundRobin      as RR
import qualified Data.Text            as T
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)
import           Network.HTTP.Client

{-| Common environment for Elasticsearch calls. Connections will be
    pipelined according to the provided HTTP connection manager.
-}
data BHEnv = BHEnv { bhServer      :: Server
                   , bhManager     :: Manager
                   , bhRequestHook :: Request -> IO Request
                   -- ^ Low-level hook that is run before every request is sent. Used to implement custom authentication strategies. Defaults to 'return' with 'mkBHEnv'.
                   }

instance (Functor m, Applicative m, MonadIO m) => MonadBH (ReaderT BHEnv m) where
  getBHEnv = ask

{-| 'Server' is used with the client functions to point at the ES instance
-}
newtype Server = Server (RR.RoundRobin Request) deriving (Eq, Generic, Typeable)

{- | make a 'Server' without cluster sniffing
-}
newServerNoSniff :: String -> IO Server
newServerNoSniff addr = do
    req <- parseUrl addr
    rr <- RR.newRoundRobin [req]
    return (Server rr)

{- | make a 'Server' with automatic cluster sniffing, see
    <https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-nodes-info.html>
-}
newServer :: String
    -> Int      -- ^ sniffing frequence in microsecond.
    -> Manager  -- ^ a HTTP 'Manager' is needed here, because cluster sniffing needs make HTTP request.
    -> IO Server
newServer addr freq mgr = do
    req <- parseUrl addr
    let req' = req{path = "/_nodes"}
    rr <- sniff req' >>= RR.newRoundRobin
    _ <-forkIO $ forever (threadDelay freq >> sniff req' >>= RR.set rr)
    return (Server rr)
  where
    sniff req = do
        res <- httpLbs req mgr
        let hosts = do
                JSON.Object o <- JSON.decode (responseBody res)
                JSON.Object o' <- HM.lookup "nodes" o
                forM (HM.elems o') $ \ (JSON.Object o'') -> do
                    (JSON.String addr') <- HM.lookup "http_address" o''
                    parseUrl (T.unpack $ "http://" `T.append` addr')
        case hosts of
            Nothing   -> error "bloodhound: sniffing failed"
            Just []   -> error "bloodhound: sniffing no nodes"
            Just reqs -> return reqs

{- | default sniffing frequence: 15mins(15 * 60 * 1000000us).
-}
defSniffFreq :: Int
defSniffFreq = 15 * 60 * 1000000


{-| All API calls to Elasticsearch operate within
    MonadBH
    . The idea is that it can be easily embedded in your
    own monad transformer stack. A default instance for a ReaderT and
    alias 'BH' is provided for the simple case.
-}
class (Functor m, Applicative m, MonadIO m) => MonadBH m where
  getBHEnv :: m BHEnv

